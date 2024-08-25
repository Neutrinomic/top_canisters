import BTree "mo:stableheapbtreemap/BTree";
import Nat64 "mo:base/Nat64";

module {

    public type Mem = {
        main : BTree.BTree<Nat64, WhaleEvent>;
    };

    public type ScanResult = BTree.ScanLimitResult<Nat64, WhaleEvent>;

    public type WhaleEvent = {
        ledger : Principal;
        amount: Nat;
        move: { #mint; #tin; #tout; #burn; };
        time: Nat32;
        owner: Principal;
        txid: Nat;
    };

    public func Mem() : Mem {
        {
            main = BTree.init<Nat64, WhaleEvent>(?32);
        };
    };

    public class Events({mem : Mem}) {

        var idx = 0;

        public func add(event : WhaleEvent) {
            
            // Convert time to unix timestamp, move to the left 32 bits and add the index
            // this will ensure we have a unique key for each event
            // and they will be in the correct order

            let key = ( Nat64.fromNat32(event.time) << 32 ) | Nat64.fromNat(idx);
            ignore BTree.insert<Nat64, WhaleEvent>(mem.main, Nat64.compare, key, event);
        };

        public func getEventsFwd(from : Nat64, length:Nat) : ScanResult {
            BTree.scanLimit<Nat64, WhaleEvent>(mem.main, Nat64.compare, from, ^0, #fwd, length);
        };

        public func getEventsBwd(from : Nat64, length:Nat) : ScanResult {
            BTree.scanLimit<Nat64, WhaleEvent>(mem.main, Nat64.compare, from, ^0, #bwd, length);
        };
       
    };

}