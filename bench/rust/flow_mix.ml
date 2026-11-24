type t = {
  mutable ipv6 : int;
  mutable vlan : int;
  mutable other : int;
  mutable rejected : int;
}

let state = { ipv6 = 0; vlan = 0; other = 0; rejected = 0 }

let reset () =
  state.ipv6 <- 0;
  state.vlan <- 0;
  state.other <- 0;
  state.rejected <- 0

let pp ppf t =
  Fmt.pf ppf "IPv6 %d, VLAN %d, other %d, IPv4 rejected %d" t.ipv6 t.vlan
    t.other t.rejected
