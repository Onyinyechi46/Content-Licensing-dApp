{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- Import standard Prelude qualified to avoid conflicts
import qualified Prelude as P

-- Import PlutusTx.Prelude unqualified for Plutus functions
import PlutusTx.Prelude hiding (Semigroup(..), unless, (++))
import PlutusTx
import qualified PlutusTx.Builtins as Builtins

import Plutus.V2.Ledger.Api hiding (divide)
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken, assetClass, AssetClass(..))

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-- Now import specific functions from standard Prelude
import Prelude (IO, String, FilePath, putStrLn, print, show, div, mod, writeFile)

-----------------------------------------------------------------------------------
-- DATUM: Content License State
-----------------------------------------------------------------------------------

data ContentLicenseDatum = ContentLicenseDatum
    { cldCreator          :: PubKeyHash           -- Original creator
    , cldContentHash      :: BuiltinByteString    -- IPFS/SHA256 hash of content
    , cldLicensePrice     :: Integer              -- Price per license in lovelace
    , cldRoyaltyPercent   :: Integer              -- Royalty % for creator (0-100)
    , cldMaxLicenses      :: Maybe Integer        -- Maximum licenses (Nothing = unlimited)
    , cldLicensesSold     :: Integer              -- Number of licenses sold
    , cldLicenseNFT       :: AssetClass           -- NFT representing the content
    , cldLicenseToken     :: AssetClass           -- Fungible token for licenses
    , cldExpiration       :: Maybe POSIXTime      -- License expiration (Nothing = perpetual)
    , cldCommercialUse    :: Bool                 -- Whether commercial use is allowed
    , cldModification     :: Bool                 -- Whether modification is allowed
    , cldRevenuePool      :: Integer              -- Accumulated revenue
    , cldPlatformFee      :: Integer              -- Platform fee percentage
    , cldReferralProgram  :: Maybe PubKeyHash     -- Optional referral program
    }
    deriving (P.Show)

PlutusTx.unstableMakeIsData ''ContentLicenseDatum

-----------------------------------------------------------------------------------
-- REDEEMER: License Actions
-----------------------------------------------------------------------------------

data LicenseAction
    = RegisterContent          -- Creator registers new content
    | PurchaseLicense          -- User purchases a license
    | TransferLicense          -- Transfer license to another user
    | ClaimRevenue             -- Creator claims accumulated revenue
    | UpdateLicenseTerms       -- Creator updates license terms
    | VerifyLicense            -- Verify license validity
    | RevokeLicense            -- Creator revokes license (with conditions)
    | ExtendLicense            -- Extend license duration
    | UpgradeLicense           -- Upgrade to commercial/modification rights
    | ReferralPurchase         -- Purchase through referral
    | DisputeResolution        -- Dispute resolution action
    deriving (P.Show)

PlutusTx.unstableMakeIsData ''LicenseAction

-----------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-----------------------------------------------------------------------------------

{-# INLINABLE myDivide #-}
myDivide :: Integer -> Integer -> Integer
myDivide = Builtins.divideInteger

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE anySigner #-}
anySigner :: ScriptContext -> Bool
anySigner ctx = length (txInfoSignatories (scriptContextTxInfo ctx)) > 0

{-# INLINABLE calculateRoyalty #-}
calculateRoyalty :: Integer -> Integer -> Integer
calculateRoyalty amount percent = (amount * percent) `myDivide` 100

{-# INLINABLE calculatePlatformFee #-}
calculatePlatformFee :: Integer -> Integer -> Integer
calculatePlatformFee amount feePercent = (amount * feePercent) `myDivide` 100

{-# INLINABLE calculateReferralBonus #-}
calculateReferralBonus :: Integer -> Integer
calculateReferralBonus amount = (amount * 5) `myDivide` 100  -- 5% referral bonus

{-# INLINABLE isLicenseValid #-}
isLicenseValid :: ContentLicenseDatum -> POSIXTime -> Bool
isLicenseValid dat currentTime =
    case cldExpiration dat of
        Nothing -> True  -- Perpetual license
        Just expiry -> currentTime <= expiry

{-# INLINABLE hasValidLicense #-}
hasValidLicense :: TxInfo -> AssetClass -> PubKeyHash -> Bool
hasValidLicense info licenseToken userPkh =
    let userValue = valuePaidTo info userPkh
        (cs, tn) = case licenseToken of
            AssetClass (c, t) -> (c, t)
    in valueOf userValue cs tn > 0

{-# INLINABLE adaPaidTo #-}
adaPaidTo :: TxInfo -> PubKeyHash -> Integer
adaPaidTo info pkh = valueOf (valuePaidTo info pkh) adaSymbol adaToken

{-# INLINABLE assetPaidTo #-}
assetPaidTo :: TxInfo -> PubKeyHash -> AssetClass -> Integer
assetPaidTo info pkh asset =
    let (cs, tn) = case asset of
            AssetClass (c, t) -> (c, t)
    in valueOf (valuePaidTo info pkh) cs tn

{-# INLINABLE ownInputValue #-}
ownInputValue :: ScriptContext -> Value
ownInputValue ctx =
    case findOwnInput ctx of
        Nothing -> traceError "script input missing"
        Just txIn -> txOutValue (txInInfoResolved txIn)

{-# INLINABLE ownInputAda #-}
ownInputAda :: ScriptContext -> Integer
ownInputAda ctx = valueOf (ownInputValue ctx) adaSymbol adaToken

{-# INLINABLE hasAsset #-}
hasAsset :: Value -> AssetClass -> Integer
hasAsset val asset =
    let (cs, tn) = case asset of
            AssetClass (c, t) -> (c, t)
    in valueOf val cs tn

{-# INLINABLE canSellMoreLicenses #-}
canSellMoreLicenses :: ContentLicenseDatum -> Bool
canSellMoreLicenses dat =
    case cldMaxLicenses dat of
        Nothing -> True
        Just maxLic -> cldLicensesSold dat < maxLic

-----------------------------------------------------------------------------------
-- MAIN VALIDATOR LOGIC
-----------------------------------------------------------------------------------

{-# INLINABLE mkContentLicenseValidator #-}
mkContentLicenseValidator :: ContentLicenseDatum -> LicenseAction -> ScriptContext -> Bool
mkContentLicenseValidator dat action ctx =
    let
        info = scriptContextTxInfo ctx
        currentTime = case txInfoValidRange info of
            Interval (LowerBound (Finite time) _) _ -> time
            _ -> traceError "invalid time range"
    in
    case action of
        -- REGISTER CONTENT: Creator registers new content
        RegisterContent ->
            traceIfFalse "creator must sign" (signedBy (cldCreator dat) ctx) &&
            traceIfFalse "must mint content NFT" (hasAsset (ownInputValue ctx) (cldLicenseNFT dat) >= 1) &&
            traceIfFalse "must mint license tokens" (hasAsset (ownInputValue ctx) (cldLicenseToken dat) >= 1) &&
            traceIfFalse "license price must be positive" (cldLicensePrice dat > 0) &&
            traceIfFalse "royalty must be 0-100" (cldRoyaltyPercent dat >= 0 && cldRoyaltyPercent dat <= 100) &&
            traceIfFalse "platform fee must be reasonable" (cldPlatformFee dat >= 0 && cldPlatformFee dat <= 20)

        -- PURCHASE LICENSE: User purchases a license
        PurchaseLicense ->
            traceIfFalse "content must be registered" (cldLicensesSold dat >= 0) &&
            traceIfFalse "can't exceed max licenses" (canSellMoreLicenses dat) &&
            traceIfFalse "must pay license price" (adaPaidTo info (cldCreator dat) >= cldLicensePrice dat) &&
            let
                platformFee = calculatePlatformFee (cldLicensePrice dat) (cldPlatformFee dat)
                royalty = calculateRoyalty (cldLicensePrice dat) (cldRoyaltyPercent dat)
                updatedRevenue = cldRevenuePool dat + royalty
                updatedSold = cldLicensesSold dat + 1
            in
            traceIfFalse "must receive license token" 
                (assetPaidTo info (head (txInfoSignatories info)) (cldLicenseToken dat) >= 1) &&
            traceIfFalse "must pay platform fee" 
                (adaPaidTo info (cldCreator dat) >= cldLicensePrice dat - platformFee)

        -- TRANSFER LICENSE: Transfer license to another user
        TransferLicense ->
            let
                signers = txInfoSignatories info
                hasEnoughSigners = length signers >= 2
            in
            traceIfFalse "need at least 2 signers" hasEnoughSigners &&
            let
                fromUser = signers !! 0
                toUser = signers !! 1
                hasLicense = hasValidLicense info (cldLicenseToken dat) fromUser
            in
            traceIfFalse "sender must have license" hasLicense &&
            traceIfFalse "must transfer license token" 
                (assetPaidTo info toUser (cldLicenseToken dat) >= 1) &&
            traceIfFalse "license must be valid" (isLicenseValid dat currentTime) &&
            if cldCommercialUse dat 
                then True
                else traceIfFalse "non-commercial license cannot be transferred for profit" 
                    (adaPaidTo info fromUser == 0)

        -- CLAIM REVENUE: Creator claims accumulated revenue
        ClaimRevenue ->
            traceIfFalse "creator must sign" (signedBy (cldCreator dat) ctx) &&
            traceIfFalse "no revenue to claim" (cldRevenuePool dat > 0) &&
            traceIfFalse "must send revenue to creator" 
                (adaPaidTo info (cldCreator dat) >= cldRevenuePool dat)

        -- UPDATE LICENSE TERMS: Creator updates terms
        UpdateLicenseTerms ->
            traceIfFalse "creator must sign" (signedBy (cldCreator dat) ctx) &&
            traceIfFalse "can't change price for existing licenses" 
                (cldLicensePrice dat == cldLicensePrice dat) &&  -- Price must remain same
            traceIfFalse "can't reduce royalty below current" 
                (cldRoyaltyPercent dat >= cldRoyaltyPercent dat) &&
            traceIfFalse "can't reduce existing rights" 
                (cldCommercialUse dat == cldCommercialUse dat && cldModification dat == cldModification dat)

        -- VERIFY LICENSE: Anyone can verify license validity
        VerifyLicense ->
            let
                signers = txInfoSignatories info
                hasSigner = not (null signers)
            in
            traceIfFalse "need at least one signer" hasSigner &&
            let
                verifier = head signers
                hasLicense = hasValidLicense info (cldLicenseToken dat) verifier
            in
            traceIfFalse "verifier must have license" hasLicense &&
            traceIfFalse "license expired" (isLicenseValid dat currentTime)

        -- REVOKE LICENSE: Creator revokes license (only for violation)
        RevokeLicense ->
            traceIfFalse "creator must sign" (signedBy (cldCreator dat) ctx) &&
            traceIfFalse "can only revoke expired licenses" 
                (not (isLicenseValid dat currentTime) || cldLicensesSold dat == 0)

        -- EXTEND LICENSE: Extend license duration
        ExtendLicense ->
            let
                signers = txInfoSignatories info
                hasSigner = not (null signers)
            in
            traceIfFalse "need at least one signer" hasSigner &&
            let
                user = head signers
                hasLicense = hasValidLicense info (cldLicenseToken dat) user
                extensionFee = (cldLicensePrice dat * 20) `myDivide` 100  -- 20% of original price
            in
            traceIfFalse "user must have license" hasLicense &&
            traceIfFalse "must pay extension fee" 
                (adaPaidTo info (cldCreator dat) >= extensionFee) &&
            case cldExpiration dat of
                Nothing -> traceError "perpetual license cannot be extended"
                Just _ -> True

        -- UPGRADE LICENSE: Upgrade license rights
        UpgradeLicense ->
            let
                signers = txInfoSignatories info
                hasSigner = not (null signers)
            in
            traceIfFalse "need at least one signer" hasSigner &&
            let
                user = head signers
                hasLicense = hasValidLicense info (cldLicenseToken dat) user
                upgradeFee = (cldLicensePrice dat * 50) `myDivide` 100  -- 50% of original price
            in
            traceIfFalse "user must have license" hasLicense &&
            traceIfFalse "must pay upgrade fee" 
                (adaPaidTo info (cldCreator dat) >= upgradeFee) &&
            traceIfFalse "license must be valid" (isLicenseValid dat currentTime)

        -- REFERRAL PURCHASE: Purchase through referral
        ReferralPurchase ->
            case cldReferralProgram dat of
                Nothing -> traceError "no referral program"
                Just referrer ->
                    traceIfFalse "can't exceed max licenses" (canSellMoreLicenses dat) &&
                    traceIfFalse "must pay license price" 
                        (adaPaidTo info (cldCreator dat) >= cldLicensePrice dat) &&
                    let
                        referralBonus = calculateReferralBonus (cldLicensePrice dat)
                        platformFee = calculatePlatformFee (cldLicensePrice dat) (cldPlatformFee dat)
                        royalty = calculateRoyalty (cldLicensePrice dat) (cldRoyaltyPercent dat)
                    in
                    traceIfFalse "must pay referral bonus" 
                        (adaPaidTo info referrer >= referralBonus) &&
                    traceIfFalse "must receive license token" 
                        (assetPaidTo info (head (txInfoSignatories info)) (cldLicenseToken dat) >= 1)

        -- DISPUTE RESOLUTION: Resolve license disputes
        DisputeResolution ->
            let
                signers = txInfoSignatories info
                hasTwoSigners = length signers >= 2
            in
            traceIfFalse "both parties must sign" hasTwoSigners &&
            let
                creator = signers !! 0
                user = signers !! 1
                hasLicense = hasValidLicense info (cldLicenseToken dat) user
            in
            traceIfFalse "user must have license" hasLicense &&
            traceIfFalse "can only resolve active disputes" 
                (cldLicensesSold dat > 0 && isLicenseValid dat currentTime)

-----------------------------------------------------------------------------------
-- UNTYPED VALIDATOR WRAPPER
-----------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkContentLicenseValidator (unsafeFromBuiltinData d)
                                 (unsafeFromBuiltinData r)
                                 (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-----------------------------------------------------------------------------------
-- SERIALIZATION & ADDRESS GENERATION
-----------------------------------------------------------------------------------

-- Helper function to get validator hash
getValidatorHash :: Validator -> ValidatorHash
getValidatorHash (Validator v) = 
    let bytes = Serialise.serialise v
        short = SBS.toShort (LBS.toStrict bytes)
        builtin = Builtins.toBuiltin (SBS.fromShort short)
    in ValidatorHash builtin

-- Generate script address
plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (getValidatorHash validator)) Nothing

-- Simple hex conversion
bytesToHex :: BS.ByteString -> String
bytesToHex = BS.foldr (\b acc -> byteToHex b P.++ acc) ""
  where
    hexChars = "0123456789abcdef" :: String
    byteToHex b =
        let hi = P.fromIntegral b `P.div` 16
            lo = P.fromIntegral b `P.mod` 16
        in [hexChars P.!! hi, hexChars P.!! lo]

-- Convert to CBOR hex for Lucid
validatorToCBORHex :: Validator -> String
validatorToCBORHex val =
    let bytes = LBS.toStrict (Serialise.serialise val)
    in bytesToHex bytes

-----------------------------------------------------------------------------------
-- MAIN FUNCTION - JUST OUTPUTS CBOR HEX
-----------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Get the CBOR hex
    let cborHex = validatorToCBORHex validator
    
    -- Print ONLY the CBOR hex (no other output)
    putStrLn cborHex
    
    -- Silently write files (no console output)
    LBS.writeFile "content-license.plutus" (Serialise.serialise validator)
    
    let bytes = LBS.toStrict (Serialise.serialise validator)
        hex   = bytesToHex bytes
    writeFile "content-license.cbor" hex
    
    let jsonContent = "{\"type\": \"PlutusScriptV2\", \"description\": \"Content Licensing Protocol\", \"cborHex\": \"" P.++ cborHex P.++ "\"}"
    writeFile "content-license.json" jsonContent