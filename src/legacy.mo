import Map "mo:map/Map";
import {bhash} "mo:map/Map";
import Principal "mo:base/Principal";

module {

    public type Mem = {
        main : Map.Map<Blob, Principal>;
    };

    public func Mem() : Mem {
        {
            main = Map.new<Blob, Principal>();
        };
    };

    public class Legacy({mem : Mem}) {
        public func add(p: Principal) {
           ignore Map.put(mem.main, bhash, Principal.toLedgerAccount(p, null), p);
        };

        public func addMapped(p: Principal, b: Blob) {
            ignore Map.put(mem.main, bhash, b, p);
        };

        public func get(b: Blob) : ?Principal {
            Map.get(mem.main, bhash, b);
        }
    }

}