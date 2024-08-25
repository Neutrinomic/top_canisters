import IcrcReader "mo:devefi-icrc-ledger/reader";
import IcpReader "mo:devefi-icp-ledger/reader";

import Principal "mo:base/Principal";
import Timer "mo:base/Timer";
import Nat64 "mo:base/Nat64";
import Nat "mo:base/Nat";

import Result "mo:base/Result";
import Option "mo:base/Option";
import ICRCLedger "mo:devefi-icrc-ledger/icrc_ledger";
import Debug "mo:base/Debug";
import SWBS "mo:swbstable/Stable";
import SWB "mo:swb";
import Array "mo:base/Array";
import Vector "mo:vector";
import RTW "./rtw";
import Time "mo:base/Time";
import Int "mo:base/Int";
import Nat32 "mo:base/Nat32";
import Events "./events";
import Legacy "./legacy";


module {
    type R<A,B> = Result.Result<A,B>;

    /// No other errors are currently possible
    public type SendError = {
        #InsuficientFunds;
    };

    
    let MAX_ACCOUNTS: Nat = 300;
    public type AccountInfo = {
        var balance: Nat;
        rtw : RTW.Mem;
        first_seen : Nat32;
    };
    
    public type Mem = {
        reader: IcrcReader.Mem;
        var accounts: Vector.Vector<(Principal, AccountInfo)>;
        var total: Nat;
        var dust_modifier : Nat;
    };

    /// Used to create new ledger memory (it's outside of the class to be able to place it in stable memory)
    public func LMem(dust_modifier: Nat) : Mem {
        {
            reader = IcrcReader.Mem();
            var accounts = Vector.new();
            var total = 0;
            var dust_modifier;
        }
    };


    /// Info about local ledger params returned by getInfo
    public type Info = {
        last_indexed_tx: Nat;
        accounts: Nat;
        
        reader_instructions_cost : Nat64;
        errors : Nat;
        lastTxTime: Nat64;
    };

    public type Meta = {
        symbol: Text;
        decimals: Nat8;
        minter: ?ICRCLedger.Account;
        fee : Nat;
    };


    /// The ledger class
    /// start_from_block should be in most cases #last (starts from the last block when first started)
    /// if something went wrong and you need to reinstall the canister
    /// or use the library with a canister that already has tokens inside it's subaccount balances
    /// you can set start_from_block to a specific block number from which you want to start reading the ledger when reinstalled
    /// you will have to remove all onRecieve, onSent, onMint, onBurn callbacks and set them again
    /// (or they could try to make calls based on old transactions)
    /// 
    /// Example:
    /// ```motoko
    ///     stable let lmem = L.LMem();
    ///     let ledger = L.Ledger(lmem, "bnz7o-iuaaa-aaaaa-qaaaa-cai", #last);
    /// ```
    public class LedgerFollower(lmem: Mem, ledger_id_txt: Text, start_from_block : ({#id:Nat; #last}), whale: Events.Events, legacy: Legacy.Legacy) {

        let ledger_id = Principal.fromText(ledger_id_txt);
        let errors = SWB.SlidingWindowBuffer<Text>();

        var reader_instructions_cost : Nat64 = 0;


        // Sender 

        var started : Bool = false;

        private func logErr(e:Text) : () {
            let idx = errors.add(e);
            if ((1+idx) % 300 == 0) { // every 300 elements
                errors.delete( errors.len() - 100 ) // delete all but the last 100
            };
        };

        private func handle_incoming_amount(owner: Principal, amount: Nat, time: Time.Time, txid:Nat) : () {
            let ownerT = Principal.toText(owner);
            if (ownerT.size() > 28) return; // We don't want to store self authenticating principals, only canisters
            
            // if (amount < lmem.total / 1000000) return; // We don't want to store accounts with very small amounts

            switch(Vector.firstIndexWith<(Principal, AccountInfo)>(lmem.accounts, func (a) = a.0 == owner)) {
                case (?idx) {
                  // update
                    let (k, v) = Vector.get<(Principal, AccountInfo)>(lmem.accounts, idx);
                    v.balance := v.balance + amount;
                    if (amount > lmem.dust_modifier) {
                        whale.add({
                            time = ts(time);
                            ledger = Principal.fromText(ledger_id_txt);
                            move = #tin;
                            amount;
                            owner = owner;
                            txid;
                        });
                    };
                    let rtwi = RTW.RTW({mem = v.rtw});
                    rtwi.updateVolume(0, time, amount);
                    // Vector.put(lmem.accounts, idx, (k, v + amount));
                };
                case (null) {
                  //add
                  let acc : AccountInfo = {
                    var balance = amount;
                    rtw = RTW.Mem(2);
                    first_seen = ts(time);
                  };
                  let rtwi = RTW.RTW({mem = acc.rtw});
                  rtwi.updateVolume(0, time, amount);
                  if (amount > lmem.dust_modifier) {
                  whale.add({
                        time = ts(time);
                        ledger = Principal.fromText(ledger_id_txt);
                        move = #tin;
                        amount;
                        owner = owner;
                        txid;
                    });
                  };
                  Vector.add(lmem.accounts, (owner, acc));
                }
            };

        };

        private func handle_outgoing_amount(owner: Principal, amount: Nat, time:Time.Time, txid:Nat) : () {
            let ownerT = Principal.toText(owner);
            if (ownerT.size() > 28) return; // We don't want to store self authenticating principals, only canisters
            
  
            switch(Vector.firstIndexWith<(Principal, AccountInfo)>(lmem.accounts, func (a) = a.0 == owner)) {
                    case (?idx) {
                    // update
                        let (k, v) = Vector.get<(Principal, AccountInfo)>(lmem.accounts, idx);
                        let new_balance = if (v.balance < amount) 0 else v.balance - amount:Nat;
                        v.balance := new_balance;
                        let rtwi = RTW.RTW({mem = v.rtw});
                        rtwi.updateVolume(1, time, amount);
                        if (amount > lmem.dust_modifier) {
                        whale.add({
                                time = ts(time);
                                ledger = Principal.fromText(ledger_id_txt);
                                move = #tout;
                                amount;
                                owner = owner;
                                txid;
                            });
                        };
                    };
                    case (null) ();
                };
                

        };

        public func ts(t:Time.Time) : Nat32 {
            Nat32.fromNat(Int.abs(t / 1000000000));
        };

       
        let IS_ICP = Principal.fromText("ryjl3-tyaaa-aaaaa-aaaba-cai") == ledger_id;

        // ICP Reader
        let reader = if (IS_ICP) { IcpReader.Reader({
            mem = lmem.reader;
            ledger_id;
            start_from_block;
            onError = logErr; // In case a cycle throws an error
            onCycleEnd = func (i: Nat64) { reader_instructions_cost := i }; // returns the instructions the cycle used. 
                                                        // It can include multiple calls to onRead
            onRead = func (transactions, fromBlock) {
                
                var idx = 0;
                label txloop for (tx in transactions.vals()) {
                    switch(tx) {
                        case (#u_mint(t)) {
                            lmem.total += t.amount;
                            let ?owner = legacy.get(t.to) else continue txloop;
                            handle_incoming_amount(owner, t.amount, Nat64.toNat(t.timestamp), fromBlock + idx);
                            if (t.amount > lmem.dust_modifier) {
                                whale.add({
                                    time = ts(Nat64.toNat(t.timestamp));
                                    ledger = Principal.fromText(ledger_id_txt);
                                    owner = owner;
                                    move = #mint;
                                    amount = (t.amount);
                                    txid = fromBlock + idx;
                                });
                            };
                        };
                        case (#u_transfer(t)) {
                            ignore do ? {
                            handle_incoming_amount(legacy.get(t.to)!, t.amount, Nat64.toNat(t.timestamp),fromBlock + idx);
                            };
                            ignore do ? {
                            handle_outgoing_amount(legacy.get(t.from)!, t.amount + t.fee, Nat64.toNat(t.timestamp),fromBlock + idx);
                            };
                        
                        };
                        case (#u_burn(t)) {    
                            let ?owner = legacy.get(t.from) else continue txloop;
                            handle_outgoing_amount(owner, t.amount, Nat64.toNat(t.timestamp), fromBlock + idx);
                            if (t.amount > lmem.dust_modifier) {
                                whale.add({
                                    time = ts(Nat64.toNat(t.timestamp));
                                    ledger = Principal.fromText(ledger_id_txt);
                                    move = #burn;
                                    amount = t.amount;
                                    owner = owner;
                                    txid = fromBlock + idx;
                                });
                            };
                        };
                        case (_) continue txloop;
                    };
              
                    idx += 1;
                };

                // Sort balances
                Vector.sort<(Principal, AccountInfo)>(lmem.accounts, func (a, b) = Nat.compare(a.1.balance, b.1.balance));
                Vector.reverse(lmem.accounts);

                // Remove accounts if there are too many

                let new_vector = Vector.new<(Principal, AccountInfo)>();
                var num = 0;
                label adding for (a in Vector.vals(lmem.accounts)) {
                    if (a.1.balance < lmem.dust_modifier) break adding;
                    if (num >= MAX_ACCOUNTS) break adding;
                    legacy.add(a.0);
                    Vector.add(new_vector, a);
                    num += 1;
                };
                lmem.accounts := new_vector;
            };
        });

        // ICRC READER
        } else { IcrcReader.Reader({
            mem = lmem.reader;
            ledger_id;
            start_from_block;
            onError = logErr; // In case a cycle throws an error
            onCycleEnd = func (i: Nat64) { reader_instructions_cost := i }; // returns the instructions the cycle used. 
                                                        // It can include multiple calls to onRead
            onRead = func (transactions: [IcrcReader.Transaction], fromBlock:Nat) {
                
                var idx = 0;
                label txloop for (tx in transactions.vals()) {
                    if (not Option.isNull(tx.mint)) {
                        let ?mint = tx.mint else continue txloop;
                        lmem.total += mint.amount;
                        handle_incoming_amount(mint.to.owner, mint.amount, Nat64.toNat(tx.timestamp), fromBlock + idx);
                        if (mint.amount > lmem.dust_modifier) {
                            whale.add({
                                time = ts(Nat64.toNat(tx.timestamp));
                                ledger = Principal.fromText(ledger_id_txt);
                                owner = mint.to.owner;
                                move = #mint;
                                amount = (mint.amount);
                                txid = fromBlock + idx;
                            });
                        };
                    };
                    if (not Option.isNull(tx.transfer)) {
                        let ?tr = tx.transfer else continue txloop;
                        let ?fee = tr.fee else continue txloop;
                        if (tr.to.owner == tr.from.owner) {
                            idx += 1;
                            continue txloop;
                         }; // inter canister transfers shouldn't be counted
                        handle_incoming_amount(tr.to.owner, tr.amount, Nat64.toNat(tx.timestamp),fromBlock + idx);
                        handle_outgoing_amount(tr.from.owner, tr.amount + fee, Nat64.toNat(tx.timestamp),fromBlock + idx);
                    };
                    if (not Option.isNull(tx.burn)) {
                        let ?burn = tx.burn else continue txloop;
                      
                        handle_outgoing_amount(burn.from.owner, burn.amount, Nat64.toNat(tx.timestamp), fromBlock + idx);
                        if (burn.amount > lmem.dust_modifier) {
                            whale.add({
                                time = ts(Nat64.toNat(tx.timestamp));
                                ledger = Principal.fromText(ledger_id_txt);
                                move = #burn;
                                amount = burn.amount;
                                owner = burn.from.owner;
                                txid = fromBlock + idx;
                            });
                        };
                    };

                    idx += 1;
                };

                // Sort balances
                Vector.sort<(Principal, AccountInfo)>(lmem.accounts, func (a, b) = Nat.compare(a.1.balance, b.1.balance));
                Vector.reverse(lmem.accounts);

                // Remove accounts if there are too many
                let new_vector = Vector.new<(Principal, AccountInfo)>();
                var num = 0;
                label adding for (a in Vector.vals(lmem.accounts)) {
                    if (a.1.balance < lmem.dust_modifier) break adding;
                    if (num >= MAX_ACCOUNTS) break adding;
                    legacy.add(a.0);
                    Vector.add(new_vector, a);
                    num += 1;
                };
                lmem.accounts := new_vector;
            };
        });
        };


        public func stop() : () {
            reader.stop();
        };

        // will loop until the actor_principal is set
        private func delayed_start() : async () {
            realStart<system>();
        };

        /// Start the ledger timers
        public func start<system>() : () {
            ignore Timer.setTimer<system>(#seconds 0, delayed_start);
        };

        /// Really starts the ledger and the whole system
        private func realStart<system>() {
            if (started) Debug.trap("already started");
            started := true;
            reader.start<system>();
        };

        /// Returns the errors that happened
        public func getErrors() : [Text] {
            let start = errors.start();
            Array.tabulate<Text>(errors.len(), func (i:Nat) {
                let ?x = errors.getOpt(start + i) else Debug.trap("memory corruption");
                x
            });
        };
    
        /// Returns info about ledger library
        public func getInfo() : Info {
            {
                last_indexed_tx = lmem.reader.last_indexed_tx;
                accounts = Vector.size(lmem.accounts);
                reader_instructions_cost;
                errors = errors.len();
                lastTxTime = reader.getReaderLastTxTime();
            }
        };


        public func dumpAccounts() : [(Principal, Nat, Nat, Nat, Nat, Nat, Nat32)] {
            Array.map<(Principal, AccountInfo),(Principal, Nat, Nat, Nat, Nat, Nat, Nat32)>(Vector.toArray(lmem.accounts), func(a) {
                let rtwi = RTW.RTW({mem = a.1.rtw});
                let volIn = rtwi.getPairVolume(0);
                let volOut = rtwi.getPairVolume(1);
                (a.0, a.1.balance, volIn.0, volIn.1, volOut.0, volOut.1, a.1.first_seen)
            });
        };

    };


}