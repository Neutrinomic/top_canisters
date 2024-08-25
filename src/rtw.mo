import Iter "mo:base/Iter";
import Time "mo:base/Time";
import Option "mo:base/Option";
import Array "mo:base/Array";
import Int "mo:base/Int";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

module {
    public type Volume = Nat;
    public type VolumeWindow = {
        lastMove : Time.Time;
        total : Volume;
        window : [Volume] // 24 hours back from lastMove
    };

    public type Mem = {
        pairs : [var ?VolumeWindow];
    };

    public func Mem(size:Nat) : Mem {
        {
            pairs = Array.init(size, null);
        };
    };

    public class RTW({
        mem : Mem;
    }) {

        // Update volume for a pair
        public func updateVolume(key : Nat, tns : Time.Time, volume_to_add : Volume) {
            let t = tns / 1000000;
            let prev = Option.get(mem.pairs[key], { lastMove = 0; total = 0; window = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] } : VolumeWindow);

            let hours_passed = (t - prev.lastMove) / (60 * 60 * 1000);

            if (hours_passed < 0) return; // There is an error, but we don't want to stop the swap from going through

            // shift window data based on hours passed
            let new_window : [var Nat] = Array.init(24, 0); // Creates new empty array with zeroes
            let start_index = Int.abs(hours_passed);

            // Only takes the data from the previous window which is in the current 24 hour window
            if (hours_passed <= 23) for (i in Iter.range(start_index, 23)) {
                new_window[i - start_index] := prev.window[i];
            };

            // add new volume to last hour
            new_window[23] := new_window[23] + volume_to_add;

            let new_vol : VolumeWindow = {
                lastMove = if (hours_passed > 0) t else prev.lastMove;
                total = prev.total + volume_to_add;
                window = Array.freeze(new_window);
            };

            mem.pairs[key] := ?new_vol;
            
        };

        // New canister function. Returns key, total volume in last 24 hours, and total volume since start of tracking.
        public func getPairVolume(key: Nat) : (Volume, Volume) {
            let t = Time.now() / 1000000;

            let ?vol = mem.pairs[key] else return (0,0);
            let hours_passed = (t - vol.lastMove) / (60 * 60 * 1000);
            assert (hours_passed >= 0);
            var sum24 = 0;
            let start_index = Int.abs(hours_passed);
            if (hours_passed <= 23) for (i in Iter.range(start_index, 23)) {
                sum24 := sum24 + vol.window[i];
            };

            (sum24, vol.total);
        };

    };

};
