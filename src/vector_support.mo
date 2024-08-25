import Blob "mo:base/Blob";
import Iter "mo:base/Iter";
import I "mo:itertools/Iter";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";

module {

    
    public func getDVectorSubaccount(vid : Nat32, who : { #source; #destination }) : Blob {
        let whobit : Nat8 = switch (who) {
            case (#source) 1;
            case (#destination) 2;
        };
        Blob.fromArray(Iter.toArray(I.pad(I.flattenArray<Nat8>([[whobit], ENat32(vid)]), 32, 0 : Nat8)));
    };


    private func ENat32(value : Nat32) : [Nat8] {
        return [
            Nat8.fromNat(Nat32.toNat(value >> 24)),
            Nat8.fromNat(Nat32.toNat((value >> 16) & 255)),
            Nat8.fromNat(Nat32.toNat((value >> 8) & 255)),
            Nat8.fromNat(Nat32.toNat(value & 255)),
        ];
    };
}