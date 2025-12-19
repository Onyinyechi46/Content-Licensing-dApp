import {
  Lucid,
  Blockfrost,
  Constr,
  Data,
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

let lucid;
let walletAddress;
let scriptAddress;

/* ===============================
   PLUTUS V2 VALIDATOR (CBOR HEX)
   =============================== */

const YIELD_VALIDATOR_CBOR ={ 
  type: "PlutusV2",
  script:   "591b5b010000323232323232323322323232323232323233223233223232323232323232323232323232323322323232323232323232323232323232322323232323232323232323232323223223232533532323232323232323232323232323232533333333333500a153355335330253500922002350102222222222222200e1054133573892011163726561746f72206d757374207369676e00053153355335333573466e24d40408888888888888800d200005305410541335738921136e6f20726576656e756520746f20636c61696d0005315335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c4d40408888888888888803940200f80f8d40408888888888888800c14c15041504cd5ce2491c6d7573742073656e6420726576656e756520746f2063726561746f720005310531053153355335333573466e20cd540a0c0e048005400920040530541054133573892116626f74682070617274696573206d757374207369676e0005315335533533302d50083501022222222222222007335303f1200150024800841504cd5ce2491675736572206d7573742068617665206c6963656e736500053153355335333573466e24d404088888888888888025200005305413302a010500110531054133573892012063616e206f6e6c79207265736f6c76652061637469766520646973707574657300053105310531533553355335333355303812001502922055500305410531054105413357389201186e656564206174206c65617374206f6e65207369676e65720005315335533533302d500835010222222222222220073502b50031054133573892011675736572206d7573742068617665206c6963656e736500053153355335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c4d40408888888888888803940200f80f8cdc199b82350102222222222222200c480a120c801053054105413357389201166d7573742070617920657874656e73696f6e20666565000531533535010222222222222220062105513263205833573892012470657270657475616c206c6963656e73652063616e6e6f7420626520657874656e64656400058105310531053153355335333573466e20d4040888888888888880252000053054105413357389211a636f6e74656e74206d757374206265207265676973746572656400053153355335302e0101054133573892011963616e277420657863656564206d6178206c6963656e73657300053153355335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c4d40408888888888888803940200f80f8d40408888888888888803014c15041504cd5ce249166d75737420706179206c6963656e736520707269636500053153355335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c54cd4d540208888888888880104d41000b08840094020d5402c88008d5402c8800520020530541054133573892011a6d7573742072656365697665206c6963656e736520746f6b656e0005315335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c4d40408888888888888803940200f80f8cdc09a80811111111111111006191919b83337040040029064009a808911111111111110011a8081111111111111100602982a082a099ab9c4901156d7573742070617920706c6174666f726d206665650005310531053105310531533535010222222222222220012153355335302f0111055133573892011963616e277420657863656564206d6178206c6963656e73657300054153355335333573466e20ccc0f8ccd54c0d44800540cd4120cc0c8d40448888888888888803940240fc0fcd40448888888888888803015015441544cd5ce249166d75737420706179206c6963656e736520707269636500054153355335333573466e20cc0c14024004c8cdc199b820014802920c801350112222222222222200c054055105513357389201176d7573742070617920726566657272616c20626f6e75730005415335333573466e20ccc0f8ccd54c0d44800540cd4120cc0c94cd4d540248888888888880104d41040b48840094024d5403488008d540348800520020540551055133573892011a6d7573742072656365697665206c6963656e736520746f6b656e000541054105410541326320583357389201136e6f20726566657272616c2070726f6772616d00058153355335330253500922002350102222222222222200e1054133573892011163726561746f72206d757374207369676e00053153355335333573466e20ccc0f4c098024d5403488008d54034880052002053054105413357389201156d757374206d696e7420636f6e74656e74204e465400053153355335333573466e20ccc0f4c098024d5403888008d54038880052002053054105413357389201186d757374206d696e74206c6963656e736520746f6b656e7300053153355335333573466e24d4040888888888888880312000053054105413357389211e6c6963656e7365207072696365206d75737420626520706f736974697665000531533553355335333573466e20d40408888888888888802d20000530541333573466e24d40408888888888888802d20c801054053105310541335738920115726f79616c7479206d75737420626520302d31303000053153355335333573466e20d40408888888888888800920000530541333573466e24d404088888888888888009202805405310531054133573892011f706c6174666f726d20666565206d75737420626520726561736f6e61626c650005310531053105310531053153355335330253500922002350102222222222222200e1054133573892011163726561746f72206d757374207369676e0005315335533553353302a01050011053105410541333573466e1cd40408888888888888802520000540531054133573892012063616e206f6e6c79207265766f6b652065787069726564206c6963656e736573000531053153355335333573466e20cd540a0c0e0480054015200405305410541335738921176e656564206174206c656173742032207369676e6572730005315335533533302d500835010222222222222220075004105413357389211873656e646572206d7573742068617665206c6963656e736500053153355335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c4cd4c0fc4800540152002500835500f2200235500f220014800814c15041504cd5ce24811b6d757374207472616e73666572206c6963656e736520746f6b656e000531533553353302a010500110541335738921156c6963656e7365206d7573742062652076616c696400053153353501022222222222222005105415335333573466e1ccc0bd40214011200005405310541335738921376e6f6e2d636f6d6d65726369616c206c6963656e73652063616e6e6f74206265207472616e7366657272656420666f722070726f666974000531053105310531053153355335330253500922002350102222222222222200e1054133573892011163726561746f72206d757374207369676e00053153355335333573466e1cd404088888888888888030d40408888888888888803015014c41504cd5ce24812863616e2774206368616e676520707269636520666f72206578697374696e67206c6963656e73657300053153355335333573466e20d40408888888888888802cd40408888888888888802c14c15041504cd5ce24812263616e27742072656475636520726f79616c74792062656c6f772063757272656e7400053153355335533535010222222222222220051350102222222222222200515335350102222222222222200510531054153353501022222222222222004135010222222222222220041533535010222222222222220041053105410531054133573892011c63616e277420726564756365206578697374696e6720726967687473000531053105310531533553355335333355303812001502922055500605410531054105413357389201186e656564206174206c65617374206f6e65207369676e65720005315335533533302d500835010222222222222220073502b50061054133573892011675736572206d7573742068617665206c6963656e736500053153355335333573466e20ccc0f4ccd54c0d04800540c9411ccc0c4d40408888888888888803940200f80f8cdc199b82350102222222222222200c4819120c801053054105413357389201146d7573742070617920757067726164652066656500053153353302a010500110541335738921156c6963656e7365206d7573742062652076616c6964000531053105310531533553355335333355303812001502922055500705410531054105413357389201186e656564206174206c65617374206f6e65207369676e65720005315335533533302d500835010222222222222220073502b50071054133573892011a7665726966696572206d7573742068617665206c6963656e736500053153353302a0105001105413357389210f6c6963656e736520657870697265640005310531053135355007222222222222005223500222533350022100113263205b335738920112696e76616c69642074696d652072616e67650005b13263205b335738920112696e76616c69642074696d652072616e67650005b135500622222222222200413550052222222222220041335303b120015001480004d5400c8888888888880104d540088888888888880104d540048888888888880104d400488008cccd5cd19b8735573aa0129000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233504704835742a01866a08e0906ae85402ccd411c124d5d0a805199aa825bae504a35742a012666aa096eb94128d5d0a80419a82382a1aba150073335504b05575a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233505f75a6ae854008c180d5d09aba250022326320663357380ce0cc0c826aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a0beeb4d5d0a80118301aba135744a004464c640cc66ae7019c1981904d55cf280089baa001357426ae8940088c98c8188cd5ce03183103009aab9e5001137540026ae854014cd411dd71aba150043335504b051200135742a006666aa096eb88004d5d0a80118299aba135744a004464c640bc66ae7017c1781704d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80498219aba135744a012464c640a066ae70144140138cccd5cd19b87500a4805084888888888880288cccd5cd19b87500b48048848888888888801c8cccd5cd19b87500c4804084888888888880088cccd5cd19b87500d4803884888888888880248cccd5cd19b87500e4803084888888888880148cccd5cd19b87500f4802884888888888880048cccd5cd19b87501048020848888888888800c8cccd5cd19b87501148018848888888888802c8cccd5cd19b8750124801084888888888880108cccd5cd19b8750134800884888888888880208cccd5cd19b8750144800084888888888880188c98c8164cd5ce02d02c82b82b02a82a02982902882802782702689a8029111111111111100389a8021111111111111100389a8019111111111111100409a8011111111111111100389a800911111111111110039999ab9a3370e6aae75404d200023333333333333322222222222222123333333333333300100f00e00d00c00b00a009008007006005004003002375c6ae85404cdd71aba15012375a6ae854044dd69aba1501033503975a6ae85403cdd69aba1500e3335504675ceb8d5d0a806999aa8233ae75c6ae854030cd40e5d69aba1500b304335742a01460866ae854024dd69aba15008375a6ae85401ccd40e5d71aba135744a00e464c6409266ae7012812411c4120584d55cf280089baa001135573a6ea80044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa0012235002222222222222533533355302112001501025335333573466e3c0380040f80f44d40d0004540cc010840f840f094cd4d400488d4008888888888888cccd4034940cc940cc940cc8ccd54c0884800540448d4004894cd54cd4ccd5cd19b8f350022200235004220020410401333573466e1cd400888004d40108800410410041004d40dc00c540d803484d4d400488004888800c4c98c80cccd5ce2491473637269707420696e707574206d697373696e67000331335014225335002210031001502211223333550023233501622333501700300100235014001500322337000029001000a4000266a02444460066004002400244a66a6a0044444444444444400c42666ae68cdc4801000816816081609299a80089a80b0011108012490350543800222333573466e24ccc04ccc01800c004d400888008d400888005200002902a25335350012222222222222200a21333573466e20d4008888888888888880240040a009c409c88ccc040cc00c00800404404488ccd54c0184800540114064cc00c00400888d4004888888888888ccd54c0444800488d40088888d401088cd400894cd4ccd5cd19b8f01700103a039133502c00600810082008502400a12233553007120012350012233550290023355300a1200123500122335502c00233350012330314800000488cc0c80080048cc0c400520000013355300712001235001223355029002333500123355300b1200123500122335502d00235500d0010012233355500801100200123355300b1200123500122335502d00235500c00100133355500300c002001111222333553004120015016335530071200123500122335502900235500900133355300412001223500222533533355300c12001323350102233350032200200200135001220011233001225335002102b1001028235001223300a0020050061003133501a004003501700133553007120012350012232335502a00330010053200135502e225335001135500a003221350022253353300c002008112223300200a004130060030023200135502722112225335001100222133005002333553007120010050040011121222300300411212223001004320013550242211225335001150142213350153004002335530061200100400132001355023221122253350011350060032213335009005300400233355300712001005004001123500122001123500122002122123300100300222333573466e3c008004068064888c8c8c004014c8004d5408888cd400520002235002225335333573466e3c0080240840804c01c0044c01800cc8004d5408488cd400520002235002225335333573466e3c00801c08007c40044c01800d2201003200135501c221225335333573466e200052000019018135005491035054360015335002135005490103505437002215335333573466e1c00d200001b01a10021335300612001001337020069001091931900c99ab9c0010191232230023758002640026aa036446666aae7c004940288cd4024c010d5d080118019aba200201a232323333573466e1cd55cea80124000466442466002006004601c6ae854008c014d5d09aba2500223263201a33573803603403026aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602e6ae854008cd403c058d5d09aba2500223263201f33573804003e03a26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931901099ab9c02202101f01e01d135573aa00226ea8004d5d0a80119a805bae357426ae8940088c98c806ccd5ce00e00d80c89aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5406088c8cccd55cf80112804119a80399aa80b98031aab9d5002300535573ca00460086ae8800c0604d5d080088910010910911980080200189119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402c66ae7005c05805004c4d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402866ae7005405004804404003c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6402066ae700440400384d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200e33573801e01c01826ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201733573803002e02a02802602402202001e26aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900819ab9c01101000e00d135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8034cd5ce00700680580509aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900719ab9c00f00e00c00b00a135573aa00226ea80048c8cccd5cd19b8750014800880148cccd5cd19b8750024800080148c98c8028cd5ce00580500400389aab9d37540022440042440022244464646666ae68cdc39aab9d5002480008cd5401cc018d5d0a80118029aba135744a004464c6401266ae7002802401c4d55cf280089baa00111221233001003002498480052410350543100223370000400222464600200244660066004004003"
};

/* ===============================
   INIT LUCID - FIXED VERSION
   =============================== */

async function initLucid() {
  try {
    log("🔄 Connecting wallet...");
    
    // First check if wallet API is available
    if (!window.cardano) {
      log("❌ No Cardano wallet detected. Please install a wallet like Lace, Eternl, or Nami.");
      return;
    }
    
    // Check specifically for Lace wallet since that's what you're using
    if (!window.cardano.lace) {
      log("❌ Lace wallet not found. Please install Lace wallet or try with Eternl/Nami.");
      log("💡 If using a different wallet, update the code to use that wallet's API");
      return;
    }
    
    log("📱 Lace wallet detected, initializing...");
    
    // Initialize Lucid
    lucid = await Lucid.new(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip"
      ),
      "Preprod"
    );

    // Enable Lace wallet
    const api = await window.cardano.lace.enable();
    lucid.selectWallet(api);

    // Get wallet address
    walletAddress = await lucid.wallet.address();
    scriptAddress = lucid.utils.validatorToAddress(script);

    // Update status indicator if it exists
    const statusDot = document.getElementById("statusDot");
    const statusText = document.getElementById("statusText");
    const connectBtn = document.getElementById("connectWallet");
    
    if (statusDot) {
      statusDot.className = "status-dot connected";
    }
    if (statusText) {
      statusText.textContent = "Connected";
    }
    if (connectBtn) {
      connectBtn.innerHTML = '<i class="fas fa-check-circle"></i> Connected';
      connectBtn.style.background = "linear-gradient(135deg, #28a745 0%, #20c997 100%)";
      connectBtn.disabled = true;
    }

    // Update wallet info display if it exists
    const walletAddressElement = document.getElementById("walletAddress");
    const scriptAddressElement = document.getElementById("scriptAddress");
    
    if (walletAddressElement) {
      walletAddressElement.textContent = walletAddress;
    }
    if (scriptAddressElement) {
      scriptAddressElement.textContent = scriptAddress;
    }

    // Show app interface (check if this element exists in your HTML)
    const appElement = document.getElementById("app");
    if (appElement) {
      appElement.classList.remove("hidden");
    }

    log("✅ Connected to Lace wallet!");
    log(`📝 Wallet address: ${walletAddress}`);
    log(`📝 Script address: ${scriptAddress}`);
    log("Ready to use Yield Lending Platform!");

  } catch (error) {
    console.error("Connection error:", error);
    log("❌ Connection failed: " + error.message);
    
    // Provide specific troubleshooting tips
    if (error.message.includes("enable")) {
      log("💡 Make sure your Lace wallet is unlocked and you've approved the connection");
    } else if (error.message.includes("network")) {
      log("💡 Make sure your wallet is connected to Preprod network");
    } else if (error.message.includes("timeout")) {
      log("💡 Connection timeout. Please try again");
    }
  }
}

/* ===============================
   DATUM
   YieldDatum
   Constr 0:
   [ lender, borrower(Maybe), principal, interest, yieldShare ]
   =============================== */

function mkYieldDatum(lender, borrowerMaybe, principal, interest, yieldShare) {
  return Data.to(
    new Constr(0, [
      lender,
      borrowerMaybe, // null OR Constr(0,[pkh])
      BigInt(principal),
      BigInt(interest),
      BigInt(yieldShare),
    ])
  );
}

/* ===============================
   REDEEMERS
   YieldAction
   =============================== */

const redeemerDeposit = () => Data.to(new Constr(0, []));
const redeemerBorrow = (amt) => Data.to(new Constr(1, [BigInt(amt)]));
const redeemerRepay = (amt) => Data.to(new Constr(2, [BigInt(amt)]));
const redeemerYield = (amt) => Data.to(new Constr(3, [BigInt(amt)]));
const Nothing = new Constr(1, []);
const Just = (v) => new Constr(0, [v]);

/* ===============================
   DEPOSIT (LENDER)
   =============================== */

async function deposit() {
  if (!lucid) {
    log("⚠️ Please connect wallet first");
    return;
  }
  
  try {
    const depositAmount = BigInt(document.getElementById('depositAmt').value) * 1_000_000n;
    const pkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

    const datum = mkYieldDatum(
      pkh,
      Nothing,
      depositAmount,
      500_000,
      70
    );

    log("🔄 Building deposit transaction...");
    const tx = await lucid
      .newTx()
      .payToContract(scriptAddress, { inline: datum }, { lovelace: depositAmount })
      .complete();

    const signed = await tx.sign().complete();
    const txHash = await signed.submit();

    log("✅ Deposit successful! TX: " + txHash);
  } catch (error) {
    log("❌ Deposit failed: " + error.message);
  }
}

/* ===============================
   BORROW (FIRST TIME LOCKS BORROWER)
   =============================== */

async function borrow() {
  if (!lucid) {
    log("⚠️ Please connect wallet first");
    return;
  }
  
  try {
    let borrowAmt = BigInt(document.getElementById("borrowAmt").value);
    borrowAmt = borrowAmt * 1_000_000n;

    const borrowerPkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;
    const borrowerAddr = await lucid.wallet.address();

    const utxos = await lucid.utxosAt(scriptAddress);
    const userUtxos = await lucid.wallet.getUtxos();

    // Find open loans
    const openLoans = utxos.filter((u) => {
      if (!u.datum) return false;
      const d = Data.from(u.datum);
      return (
        u.assets.lovelace >= borrowAmt &&
        d.fields[1].fields.length == 0 &&
        d.fields[0] !== borrowerPkh
      );
    });

    if (openLoans.length === 0) {
      log("❌ No available loans found");
      return;
    }

    const loanUtxo = openLoans[0];
    const d = Data.from(loanUtxo.datum);
    const [lender, , principal, interest, yieldShare] = d.fields;

    if (borrowAmt > loanUtxo.assets.lovelace) {
      log("❌ Borrow amount exceeds available loan");
      return;
    }

    if (lender === borrowerPkh) {
      log("❌ Cannot borrow from own loan");
      return;
    }

    const newDatum = mkYieldDatum(
      lender,
      new Constr(0, [borrowerPkh]),
      principal,
      interest,
      yieldShare
    );

    const remainingAda = loanUtxo.assets.lovelace - borrowAmt;

    log("🔄 Building borrow transaction...");
    const tx = await lucid
      .newTx()
      .collectFrom([loanUtxo], redeemerBorrow(borrowAmt))
      .collectFrom(userUtxos)
      .attachSpendingValidator(script)
      .payToAddress(borrowerAddr, { lovelace: borrowAmt })
      .payToContract(
        scriptAddress,
        { inline: newDatum },
        { lovelace: remainingAda }
      )
      .addSignerKey(borrowerPkh)
      .complete();

    const signed = await tx.sign().complete();
    const txHash = await signed.submit();

    log("✅ Borrow successful! TX: " + txHash);
  } catch (error) {
    log("❌ Borrow failed: " + error.message);
  }
}

/* ===============================
   REPAY (BORROWER ONLY)
   =============================== */

async function repay() {
  if (!lucid) {
    log("⚠️ Please connect wallet first");
    return;
  }
  
  try {
    const borrowerPkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;
    const utxos = await lucid.utxosAt(scriptAddress);
    const userUtxos = await lucid.wallet.getUtxos();

    // Find loan owned by this borrower
    const loanUtxo = utxos.find((u) => {
      if (!u.datum) return false;
      const d = Data.from(u.datum);
      const borrower = d.fields[1];
      return (
        borrower && 
        borrower.fields && 
        borrower.fields.length > 0 &&
        borrower.fields[0] === borrowerPkh
      );
    });

    if (!loanUtxo) {
      log("❌ No active loan found for this wallet");
      return;
    }

    const datum = Data.from(loanUtxo.datum);
    const [lender, borrower, principal, interest, yieldShare] = datum.fields;
    const totalRepayment = BigInt(principal) + BigInt(interest);

    const userGoodUTxo = userUtxos.find((u) => u.assets.lovelace >= totalRepayment);

    if (!userGoodUTxo) {
      log("❌ Insufficient funds for repayment");
      return;
    }

    const resetDatum = mkYieldDatum(
      lender,
      borrower,
      principal,
      interest,
      yieldShare
    );

    log("🔄 Building repayment transaction...");
    const tx = await lucid
      .newTx()
      .collectFrom([userGoodUTxo])
      .collectFrom([loanUtxo], redeemerRepay(totalRepayment))
      .attachSpendingValidator(script)
      .payToContract(
        scriptAddress,
        { inline: resetDatum },
        { lovelace: loanUtxo.assets.lovelace + totalRepayment }
      )
      .addSignerKey(borrowerPkh)
      .complete();

    const signed = await tx.sign().complete();
    const txHash = await signed.submit();

    log("✅ Repayment successful! TX: " + txHash);
  } catch (error) {
    log("❌ Repayment failed: " + error.message);
  }
}

/* ===============================
   DISTRIBUTE YIELD (LENDER)
   =============================== */

async function distributeYield() {
  if (!lucid) {
    log("⚠️ Please connect wallet first");
    return;
  }
  
  try {
    const lenderAddr = await lucid.wallet.address();
    const lenderPkh = lucid.utils.getAddressDetails(lenderAddr).paymentCredential.hash;
    
    const utxos = await lucid.utxosAt(scriptAddress);
    
    // Find a fully repaid loan owned by this lender
    const loanUtxo = utxos.find((u) => {
      if (!u.datum) return false;
      const d = Data.from(u.datum);
      const lenderDPkh = d.fields[0];
      const borrower = d.fields[1];
      const principal = d.fields[2];
      const interest = d.fields[3];
      
      return (
        borrower && 
        borrower.fields && 
        borrower.fields.length > 0 &&
        lenderPkh === lenderDPkh &&
        u.assets.lovelace >= BigInt(principal) + BigInt(interest)
      );
    });

    if (!loanUtxo) {
      log("❌ No fully repaid loan found for yield distribution");
      return;
    }

    const datum = Data.from(loanUtxo.datum);
    const [, borrowerMaybe, , , yieldShare] = datum.fields;
    const borrowerPkh = borrowerMaybe.fields[0];

    const lenderShare = (loanUtxo.assets.lovelace * BigInt(yieldShare)) / 100n;
    const borrowerShare = loanUtxo.assets.lovelace - lenderShare;
    
    const userUtxos = await lucid.wallet.getUtxos();
    const borrowerAddr = lucid.utils.credentialToAddress({
      type: "Key",
      hash: borrowerPkh,
    });

    log("🔄 Building yield distribution transaction...");
    const tx = await lucid
      .newTx()
      .collectFrom([loanUtxo], redeemerYield(loanUtxo.assets.lovelace))
      .collectFrom(userUtxos)
      .attachSpendingValidator(script)
      .payToAddress(lenderAddr, { lovelace: lenderShare })
      .payToAddress(borrowerAddr, { lovelace: borrowerShare })
      .addSigner(lenderAddr)
      .complete();

    const signed = await tx.sign().complete();
    const txHash = await signed.submit();
    
    log("✅ Yield distributed! TX: " + txHash);
  } catch (error) {
    log("❌ Yield distribution failed: " + error.message);
  }
}

/* ===============================
   UI FUNCTIONS
   =============================== */

function log(msg) {
  const logElement = document.getElementById("log");
  if (logElement) {
    const timestamp = new Date().toLocaleTimeString();
    logElement.textContent = `[${timestamp}] ${msg}\n` + logElement.textContent;
  }
}

/* ===============================
   INITIALIZE EVENT LISTENERS
   =============================== */

// Initialize when the DOM is loaded
document.addEventListener('DOMContentLoaded', function() {
  // Check if we're running in a browser environment
  if (typeof window === 'undefined') {
    console.error("This script must run in a browser environment");
    return;
  }
  
  // Set up event listeners
  const connectBtn = document.getElementById("connectWallet");
  const depositBtn = document.getElementById("depositBtn");
  const borrowBtn = document.getElementById("borrowBtn");
  const repayBtn = document.getElementById("repayBtn");
  const yieldBtn = document.getElementById("yieldBtn");
  
  if (connectBtn) {
    connectBtn.onclick = initLucid;
    log("Click 'Connect Wallet' to begin");
  } else {
    console.error("Connect wallet button not found!");
  }
  
  if (depositBtn) depositBtn.onclick = deposit;
  if (borrowBtn) borrowBtn.onclick = borrow;
  if (repayBtn) repayBtn.onclick = repay;
  if (yieldBtn) yieldBtn.onclick = distributeYield;
  
  // Log initial state
  log("Yield Lending Platform Ready");
  log("Note: Using Preprod network with Lace wallet");
});

// Add global error handler
window.addEventListener('error', function(e) {
  log("⚠️ JavaScript Error: " + e.message);
});