module WireGen.Lib3d

friend EverParse3d.Prelude

let nlist_u8 n c s = WireGen.Lib.fldata_u8_list (FStar.UInt32.v n) s
