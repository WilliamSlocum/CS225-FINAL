module TermPairSet = struct
 include Set.Make(struct type t = term * term let compare = Pervasives.compare end)
end
type term_pair_set = TermPairSet.t
