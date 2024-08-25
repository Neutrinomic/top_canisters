import LedgerFollower "./ledger_follower";
import Vector "mo:vector";

import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Map "mo:map/Map";
import {phash} "mo:map/Map";
import SWBS "mo:swbstable/Stable";
import Result "mo:base/Result";
import Nat "mo:base/Nat";
import Events "./events";
import Legacy "./legacy";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import VS "./vector_support";

actor {
    type R<A,B> = Result.Result<A,B>;

    type KnownCanisters = {
        sns: ?Principal;
        name: Text;
        appurl: ?Text;
        icrc: [Nat32];
        hidden: Bool;
    };

    stable let whale_events_memx = Events.Mem();
    let whale_events = Events.Events({mem = whale_events_memx});

    stable let legacy_mem = Legacy.Mem();
    let legacy = Legacy.Legacy({mem = legacy_mem});

    stable let known_canisters = Map.new<Principal, KnownCanisters>();

    type Ledger = {
        id: Principal;
        mem: LedgerFollower.Mem;
    };

    type LedgersResult = {
        id: Principal;
        accounts: [(Principal, Nat, Nat,Nat,Nat,Nat, Nat32)];
        reader: LedgerFollower.Info;
    };

    stable let ledgers = Vector.new<Ledger>();

    let followers = Vector.new<LedgerFollower.LedgerFollower>();
    for (ledger in Vector.vals(ledgers)) {
        let follower = LedgerFollower.LedgerFollower(ledger.mem, Principal.toText(ledger.id), #id(0), whale_events, legacy);
        follower.start<system>();
        Vector.add(followers, follower);
    };
    
    public shared({caller}) func setKnownCanisters(k : [(Principal, KnownCanisters)]) : async () {
        assert(caller == Principal.fromText("lovjp-a2s3z-lqgmk-epyel-hshnr-ksdzf-abimc-f7dpu-33z4u-2vbkf-uae"));
        for (canister in k.vals()) {
            ignore Map.put(known_canisters, phash, canister.0, canister.1);
            legacy.add(canister.0);
        };
    };

    public query func getKnownCanisters() : async [(Principal, KnownCanisters)] {
        Map.toArray(known_canisters);
    };

    
    public query func getAccounts() : async [LedgersResult] {
        var i = 0;
        let rez = Vector.new<LedgersResult>();
        while(i < Vector.size(ledgers)) {
            let ledger = Vector.get(ledgers, i);
            let follower = Vector.get(followers, i);
            Vector.add(rez, {id=ledger.id; accounts=follower.dumpAccounts(); reader = follower.getInfo()});
            i += 1;
        };

        Vector.toArray(rez);
        
    };

    public query func getErrors() : async ([[Text]]) {
        var i = 0;
        let rez = Vector.new<[Text]>();
        while(i < Vector.size(ledgers)) {
            let ledger = Vector.get(ledgers, i);
            let follower = Vector.get(followers, i);
            Vector.add(rez, follower.getErrors());
            i += 1;
        };

        Vector.toArray(rez);
    };

    public shared({caller}) func add_ledger(id: Principal, dust_modifier: Nat) {
        assert(caller == Principal.fromText("lovjp-a2s3z-lqgmk-epyel-hshnr-ksdzf-abimc-f7dpu-33z4u-2vbkf-uae"));
        // Check if the ledger is already added
        for (ledger in Vector.vals(ledgers)) {
            if (ledger.id == id) return;
        };
        let new_mem = LedgerFollower.LMem(dust_modifier);
        let new_follower = LedgerFollower.LedgerFollower(new_mem, Principal.toText(id), #id(0), whale_events, legacy);
        new_follower.start<system>();
        Vector.add(ledgers, {id; mem=new_mem});
        Vector.add(followers, new_follower);
    };

    public shared({caller}) func rem_ledger(id: Principal) {
        assert(caller == Principal.fromText("lovjp-a2s3z-lqgmk-epyel-hshnr-ksdzf-abimc-f7dpu-33z4u-2vbkf-uae"));
        let old_ledgers = Vector.clone<Ledger>(ledgers);
        let old_followers = Vector.clone<LedgerFollower.LedgerFollower>(followers);
        Vector.clear(ledgers);
        Vector.clear(followers);
        for (i in Iter.range(0, Vector.size(old_ledgers) - 1)) {
            let ledger = Vector.get(old_ledgers, i);
            let follower = Vector.get(old_followers, i);
            if (ledger.id != id) {
                Vector.add(ledgers, ledger);
                Vector.add(followers, follower);
            } else {
                follower.stop();
            }
        };
    };

    public query func getEventsFwd(from : Nat64, length:Nat) : async Events.ScanResult {
        whale_events.getEventsFwd(from, length);
    };

    public query func getEventsBwd(from : Nat64, length:Nat) : async Events.ScanResult {
        whale_events.getEventsBwd(from, length);
    };

    public query func stats() : async (Nat, Nat) {
        (   
            Map.size(legacy_mem.main),
            Map.size(known_canisters),
        )
    };

    public shared({caller}) func add_vector_addresses(id: Principal) : async () {
        assert(caller == Principal.fromText("lovjp-a2s3z-lqgmk-epyel-hshnr-ksdzf-abimc-f7dpu-33z4u-2vbkf-uae"));
        
        var idx:Nat32 = 0;
        label cycle loop {
            
            legacy.addMapped(id, Principal.toLedgerAccount(id, ?VS.getDVectorSubaccount(idx, #source)));
            legacy.addMapped(id, Principal.toLedgerAccount(id, ?VS.getDVectorSubaccount(idx, #destination)));

            idx += 1;
            if (idx > 500) break cycle;
        };
        
        
    };


    public query func ver() : async Nat{
        1
    }
}