type +'a t = Staged of 'a [@@unboxed]

let[@inline always] stage x = Staged x
let[@inline always] unstage (Staged x) = x
