(* RFC 8949 Appendix A, every item, as the cbor/test-vectors file
   appendix_a.json carries it (ocaml-cbor's test-vectors/appendix_a.json): the
   diagnostic notation, the verdict of a deterministic reader, and the hex. A
   float is refused at its head, under a tag too, and an indefinite length at
   its first indefinite head; the offset is that head's. *)
let appendix_a =
  [
    ({|0|}, `Accept, "00");
    ({|1|}, `Accept, "01");
    ({|10|}, `Accept, "0a");
    ({|23|}, `Accept, "17");
    ({|24|}, `Accept, "1818");
    ({|25|}, `Accept, "1819");
    ({|100|}, `Accept, "1864");
    ({|1000|}, `Accept, "1903e8");
    ({|1000000|}, `Accept, "1a000f4240");
    ({|1000000000000|}, `Accept, "1b000000e8d4a51000");
    ({|18446744073709551615|}, `Accept, "1bffffffffffffffff");
    ({|18446744073709551616|}, `Accept, "c249010000000000000000");
    ({|-18446744073709551616|}, `Accept, "3bffffffffffffffff");
    ({|-18446744073709551617|}, `Accept, "c349010000000000000000");
    ({|-1|}, `Accept, "20");
    ({|-10|}, `Accept, "29");
    ({|-100|}, `Accept, "3863");
    ({|-1000|}, `Accept, "3903e7");
    ({|0.0|}, `Float 0, "f90000");
    ({|-0.0|}, `Float 0, "f98000");
    ({|1.0|}, `Float 0, "f93c00");
    ({|1.1|}, `Float 0, "fb3ff199999999999a");
    ({|1.5|}, `Float 0, "f93e00");
    ({|65504.0|}, `Float 0, "f97bff");
    ({|100000.0|}, `Float 0, "fa47c35000");
    ({|3.4028234663852886e+38|}, `Float 0, "fa7f7fffff");
    ({|1e+300|}, `Float 0, "fb7e37e43c8800759c");
    ({|5.960464477539063e-08|}, `Float 0, "f90001");
    ({|6.103515625e-05|}, `Float 0, "f90400");
    ({|-4.0|}, `Float 0, "f9c400");
    ({|-4.1|}, `Float 0, "fbc010666666666666");
    ({|Infinity|}, `Float 0, "f97c00");
    ({|NaN|}, `Float 0, "f97e00");
    ({|-Infinity|}, `Float 0, "f9fc00");
    ({|Infinity|}, `Float 0, "fa7f800000");
    ({|NaN|}, `Float 0, "fa7fc00000");
    ({|-Infinity|}, `Float 0, "faff800000");
    ({|Infinity|}, `Float 0, "fb7ff0000000000000");
    ({|NaN|}, `Float 0, "fb7ff8000000000000");
    ({|-Infinity|}, `Float 0, "fbfff0000000000000");
    ({|false|}, `Accept, "f4");
    ({|true|}, `Accept, "f5");
    ({|null|}, `Accept, "f6");
    ({|undefined|}, `Accept, "f7");
    ({|simple(16)|}, `Accept, "f0");
    ({|simple(255)|}, `Accept, "f8ff");
    ( {|0("2013-03-21T20:04:00Z")|},
      `Accept,
      "c074323031332d30332d32315432303a30343a30305a" );
    ({|1(1363896240)|}, `Accept, "c11a514b67b0");
    ({|1(1363896240.5)|}, `Float 1, "c1fb41d452d9ec200000");
    ({|23(h'01020304')|}, `Accept, "d74401020304");
    ({|24(h'6449455446')|}, `Accept, "d818456449455446");
    ( {|32("http://www.example.com")|},
      `Accept,
      "d82076687474703a2f2f7777772e6578616d706c652e636f6d" );
    ({|h''|}, `Accept, "40");
    ({|h'01020304'|}, `Accept, "4401020304");
    ({|""|}, `Accept, "60");
    ({|"a"|}, `Accept, "6161");
    ({|"IETF"|}, `Accept, "6449455446");
    ({|"\"\\"|}, `Accept, "62225c");
    ({|"\u00fc"|}, `Accept, "62c3bc");
    ({|"\u6c34"|}, `Accept, "63e6b0b4");
    ({|"\ud800\udd51"|}, `Accept, "64f0908591");
    ({|[]|}, `Accept, "80");
    ({|[1, 2, 3]|}, `Accept, "83010203");
    ({|[1, [2, 3], [4, 5]]|}, `Accept, "8301820203820405");
    ( {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]|},
      `Accept,
      "98190102030405060708090a0b0c0d0e0f101112131415161718181819" );
    ({|{}|}, `Accept, "a0");
    ({|{1: 2, 3: 4}|}, `Accept, "a201020304");
    ({|{"a": 1, "b": [2, 3]}|}, `Accept, "a26161016162820203");
    ({|["a", {"b": "c"}]|}, `Accept, "826161a161626163");
    ( {|{"a": "A", "b": "B", "c": "C", "d": "D", "e": "E"}|},
      `Accept,
      "a56161614161626142616361436164614461656145" );
    ({|(_ h'0102', h'030405')|}, `Indefinite 0, "5f42010243030405ff");
    ({|"streaming"|}, `Indefinite 0, "7f657374726561646d696e67ff");
    ({|[]|}, `Indefinite 0, "9fff");
    ({|[1, [2, 3], [4, 5]]|}, `Indefinite 0, "9f018202039f0405ffff");
    ({|[1, [2, 3], [4, 5]]|}, `Indefinite 0, "9f01820203820405ff");
    ({|[1, [2, 3], [4, 5]]|}, `Indefinite 5, "83018202039f0405ff");
    ({|[1, [2, 3], [4, 5]]|}, `Indefinite 2, "83019f0203ff820405");
    ( {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]|},
      `Indefinite 0,
      "9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff" );
    ({|{"a": 1, "b": [2, 3]}|}, `Indefinite 0, "bf61610161629f0203ffff");
    ({|["a", {"b": "c"}]|}, `Indefinite 3, "826161bf61626163ff");
    ({|{"Fun": true, "Amt": -2}|}, `Indefinite 0, "bf6346756ef563416d7421ff");
  ]

(* EverCBOR's conformance vectors: share/everparse/tests/cbor/cbor_tests.c at
   EverParse v2026.11.13, built with -DEVERCBOR_DET against the prefix's
   CBORDet.o and run with CBOR_TEST_OUT_DIR set (363 of 363 passed). Each row
   is a test name, its verdict under the deterministic API and the hex of the
   input file the run wrote. [`Accept]: the C validates it and serialises the
   value back to the same bytes; [`Prefix n]: it validates the first [n]
   bytes; [`Refuse]: it refuses it; [`Refuse_text]: the bytes are a payload
   the C refuses to make a text string of, as not UTF-8. *)
let evercbor =
  [
    ("uint_zero_canonical", `Accept, "00");
    ("uint_small_canonical", `Accept, "17");
    ("uint_one_byte_canonical", `Accept, "1864");
    ("uint_two_byte_canonical", `Accept, "1903e8");
    ("uint_four_byte_canonical", `Accept, "1a000f4240");
    ("uint_eight_byte_canonical", `Accept, "1b0000000100000000");
    ("uint_zero_nondet", `Refuse, "1800");
    ("uint_100_nondet", `Refuse, "190064");
    ("uint_zero_long_nondet", `Refuse, "1b0000000000000000");
    ("neg_minus_one_canonical", `Accept, "20");
    ("neg_small_canonical", `Accept, "29");
    ("neg_one_byte_canonical", `Accept, "3863");
    ("neg_minus_one_nondet", `Refuse, "3800");
    ("bstr_empty_canonical", `Accept, "40");
    ("bstr_short_canonical", `Accept, "44deadbeef");
    ("bstr_empty_nondet", `Refuse, "5800");
    ("tstr_empty_canonical", `Accept, "60");
    ("tstr_hello_canonical", `Accept, "6568656c6c6f");
    ("tstr_hello_nondet", `Refuse, "780568656c6c6f");
    ("arr_empty_canonical", `Accept, "80");
    ("arr_three_canonical", `Accept, "83010203");
    ("arr_empty_nondet", `Refuse, "9800");
    ( "arr_deeply_nested_canonical",
      `Accept,
      "81818181818181818181818181818181818181818181818181818181818101" );
    ("map_empty_canonical", `Accept, "a0");
    ("map_two_canonical", `Accept, "a201410102626162");
    ("map_two_nondet", `Refuse, "a202626162014101");
    ( "map_deeply_nested_canonical",
      `Accept,
      "a100a100a100a100a100a100a100a100a100a10000" );
    ( "map_nested_keys_canonical",
      `Accept,
      "a282a2a2a20000010100a2020003010100a2a20400050100a2060007010101a2a2a20000010100a2020003010100a2a20400050100a20600070101010082a2a2a20000010100a2020003010100a2a20400050100a2060007010101a2a2a20800090100a20a000b010100a2a20c000d0100a20e000f01010101"
    );
    ("map_dup_key_invalid", `Refuse, "a201000101");
    ("map_dup_diff_enc_invalid", `Refuse, "a20100180101");
    ("tag_canonical", `Accept, "d903e800");
    ("tag_nondet", `Refuse, "da000003e800");
    ("simple_short_canonical", `Accept, "f0");
    ("simple_long_canonical", `Accept, "f864");
    ("simple_24_invalid", `Refuse, "f818");
    ("invalid_truncated", `Refuse, "18");
    ("invalid_bstr_short", `Refuse, "440102");
    ("invalid_arr_short", `Refuse, "830102");
    ("invalid_map_short", `Refuse, "a20102");
    ("invalid_indefinite", `Refuse, "9f01ff");
    ("uint_one_canonical", `Accept, "01");
    ("uint_ten_canonical", `Accept, "0a");
    ("uint_24_canonical", `Accept, "1818");
    ("uint_25_canonical", `Accept, "1819");
    ("uint_trillion_canonical", `Accept, "1b000000e8d4a51000");
    ("neg_two_byte_canonical", `Accept, "3903e7");
    ("tstr_a_canonical", `Accept, "6161");
    ("tstr_ietf_canonical", `Accept, "6449455446");
    ("tstr_escapes_canonical", `Accept, "62225c");
    ("tstr_u_umlaut_canonical", `Accept, "62c3bc");
    ("tstr_water_canonical", `Accept, "63e6b0b4");
    ("tstr_drachma_canonical", `Accept, "64f0908591");
    ("arr_nested_canonical", `Accept, "8301820203820405");
    ( "arr_25_canonical",
      `Accept,
      "98190102030405060708090a0b0c0d0e0f101112131415161718181819" );
    ("map_mixed_canonical", `Accept, "a26161016162820203");
    ("arr_with_map_canonical", `Accept, "826161a161626163");
    ("map_five_canonical", `Accept, "a56161614161626142616361436164614461656145");
    ("utf8_t37_4", `Accept, "622000");
    ("utf8_t37_3", `Refuse_text, "200020ff");
    ("utf8_t37_2_1", `Accept, "63200035");
    ("utf8_t37_2", `Refuse_text, "f0808080");
    ("utf8_t37_1", `Refuse_text, "e08080");
    ("utf8_t37_0", `Refuse_text, "c080");
    ("utf8_t35_0", `Refuse_text, "f4808000");
    ("utf8_t34_0", `Refuse_text, "f1808000");
    ("utf8_t33_0", `Refuse_text, "f0908000");
    ("utf8_t32_0", `Refuse_text, "ed8000");
    ("utf8_t31_0", `Refuse_text, "e08000");
    ("utf8_t30_4", `Refuse_text, "df00");
    ("utf8_t30_0", `Refuse_text, "c200");
    ("utf8_t5_0", `Accept, "6100");
    ("utf8_t36_9_1", `Accept, "68efbfbe3defbfbe2e");
    ("utf8_t36_9", `Accept, "68efbfbf3defbfbf2e");
    ("utf8_t36_10", `Refuse_text, "efbfbdefbfbdefbfbd3de080af2e");
    ("utf8_t36_8", `Refuse_text, "efbfbdefbfbdefbfbd3deda0802e");
    ("utf8_t36_7", `Refuse_text, "efbfbdefbfbdefbfbdefbfbd3df7bfbfbf2e");
    ("utf8_t9_1", `Refuse_text, "c24142");
    ("utf8_t35_3", `Refuse_text, "f48080ff");
    ("utf8_t35_2", `Refuse_text, "f48080c0");
    ("utf8_t35_1", `Refuse_text, "f480807f");
    ("utf8_t34_3", `Refuse_text, "f18080ff");
    ("utf8_t34_2", `Refuse_text, "f18080c0");
    ("utf8_t34_1", `Refuse_text, "f180807f");
    ("utf8_t33_3", `Refuse_text, "f09080ff");
    ("utf8_t33_2", `Refuse_text, "f09080c0");
    ("utf8_t33_1", `Refuse_text, "f090807f");
    ("utf8_t32_3", `Refuse_text, "ed80ff");
    ("utf8_t32_2", `Refuse_text, "ed80c0");
    ("utf8_t32_1", `Refuse_text, "ed807f");
    ("utf8_t31_3", `Refuse_text, "e080ff");
    ("utf8_t31_2", `Refuse_text, "e080c0");
    ("utf8_t31_1", `Refuse_text, "e0807f");
    ("utf8_t30_7", `Refuse_text, "dfff");
    ("utf8_t30_6", `Refuse_text, "dfc0");
    ("utf8_t30_5", `Refuse_text, "df7f");
    ("utf8_t30_3", `Refuse_text, "c2ff");
    ("utf8_t30_2", `Refuse_text, "c2c0");
    ("utf8_t30_1", `Refuse_text, "c27f");
    ("utf8_t29_9", `Refuse_text, "ff20");
    ("utf8_t29_8", `Refuse_text, "f520");
    ("utf8_t29_7", `Refuse_text, "c120");
    ("utf8_t29_6", `Refuse_text, "8120");
    ("utf8_t29_5", `Refuse_text, "208020");
    ("utf8_t29_4", `Refuse_text, "8020");
    ("utf8_t29_3", `Refuse_text, "2021212324fe");
    ("utf8_t29_2", `Refuse_text, "20212123fe20");
    ("utf8_t29_1", `Refuse_text, "2080");
    ("utf8_t29_0", `Refuse_text, "80");
    ("utf8_t28_15", `Accept, "64f48fbfbf");
    ("utf8_t28_14", `Accept, "64f3bfbfbf");
    ("utf8_t28_13", `Accept, "64f3afbfbf");
    ("utf8_t28_12", `Accept, "64f39fbfbf");
    ("utf8_t28_11", `Accept, "64f38fbfbf");
    ("utf8_t28_10", `Accept, "64f2bfbfbf");
    ("utf8_t28_9", `Accept, "64f2afbfbf");
    ("utf8_t28_8", `Accept, "64f29fbfbf");
    ("utf8_t28_7", `Accept, "64f28fbfbf");
    ("utf8_t28_6", `Accept, "64f1bfbfbf");
    ("utf8_t28_5", `Accept, "64f1afbfbf");
    ("utf8_t28_4", `Accept, "64f19fbfbf");
    ("utf8_t28_3", `Accept, "64f18fbfbf");
    ("utf8_t28_2", `Accept, "64f0bfbfbf");
    ("utf8_t28_1", `Accept, "64f0afbfbf");
    ("utf8_t28_0", `Accept, "64f09fbfbf");
    ("utf8_t27_15", `Accept, "64f48fbfbe");
    ("utf8_t27_14", `Accept, "64f3bfbfbe");
    ("utf8_t27_13", `Accept, "64f3afbfbe");
    ("utf8_t27_12", `Accept, "64f39fbfbe");
    ("utf8_t27_11", `Accept, "64f38fbfbe");
    ("utf8_t27_10", `Accept, "64f2bfbfbe");
    ("utf8_t27_9", `Accept, "64f2afbfbe");
    ("utf8_t27_8", `Accept, "64f29fbfbe");
    ("utf8_t27_7", `Accept, "64f28fbfbe");
    ("utf8_t27_6", `Accept, "64f1bfbfbe");
    ("utf8_t27_5", `Accept, "64f1afbfbe");
    ("utf8_t27_4", `Accept, "64f19fbfbe");
    ("utf8_t27_3", `Accept, "64f18fbfbe");
    ("utf8_t27_2", `Accept, "64f0bfbfbe");
    ("utf8_t27_1", `Accept, "64f0afbfbe");
    ("utf8_t27_0", `Accept, "64f09fbfbe");
    ("utf8_t26_17", `Accept, "63efb79f");
    ("utf8_t26_16", `Accept, "63efb79e");
    ("utf8_t26_15", `Accept, "63efb79d");
    ("utf8_t26_14", `Accept, "63efb79c");
    ("utf8_t26_13", `Accept, "63efb79b");
    ("utf8_t26_12", `Accept, "63efb79a");
    ("utf8_t26_11", `Accept, "63efb799");
    ("utf8_t26_10", `Accept, "63efb798");
    ("utf8_t26_9", `Accept, "63efb797");
    ("utf8_t26_8", `Accept, "63efb796");
    ("utf8_t26_7", `Accept, "63efb795");
    ("utf8_t26_6", `Accept, "63efb794");
    ("utf8_t26_5", `Accept, "63efb793");
    ("utf8_t26_4", `Accept, "63efb792");
    ("utf8_t26_3", `Accept, "63efb791");
    ("utf8_t26_2", `Accept, "63efb790");
    ("utf8_t26_1", `Accept, "63efbfbf");
    ("utf8_t26_0", `Accept, "63efbfbe");
    ("utf8_t25_7", `Refuse_text, "edafbfedbfbf");
    ("utf8_t25_6", `Refuse_text, "edafbfedb080");
    ("utf8_t25_5", `Refuse_text, "edae80edbfbf");
    ("utf8_t25_4", `Refuse_text, "edae80edb080");
    ("utf8_t25_3", `Refuse_text, "edadbfedbfbf");
    ("utf8_t25_2", `Refuse_text, "edadbfedb080");
    ("utf8_t25_1", `Refuse_text, "eda080edbfbf");
    ("utf8_t25_0", `Refuse_text, "eda080edb080");
    ("utf8_t24_7", `Refuse_text, "edbfbf");
    ("utf8_t24_6", `Refuse_text, "edbe80");
    ("utf8_t24_5", `Refuse_text, "edb080");
    ("utf8_t24_4", `Refuse_text, "edafbf");
    ("utf8_t24_3", `Refuse_text, "edae80");
    ("utf8_t24_2", `Refuse_text, "edadbf");
    ("utf8_t24_0_2", `Refuse_text, "313233eda08031");
    ("utf8_t24_0_1", `Refuse_text, "eda08035");
    ("utf8_t24_0", `Refuse_text, "eda080");
    ("utf8_t23_3", `Refuse_text, "f887bfbfbf");
    ("utf8_t23_2", `Refuse_text, "f08fbfbf");
    ("utf8_t23_1", `Refuse_text, "e09fbf");
    ("utf8_t23_0", `Refuse_text, "c1bf");
    ("utf8_t22_6", `Refuse_text, "fc80808080af");
    ("utf8_t22_5", `Refuse_text, "f8808080af");
    ("utf8_t22_4", `Refuse_text, "f08080af");
    ("utf8_t22_3", `Refuse_text, "e080af");
    ("utf8_t22_2", `Refuse_text, "c0af");
    ("utf8_t21_6", `Refuse_text, "373839fe");
    ("utf8_t21_5", `Refuse_text, "3738fe");
    ("utf8_t21_4", `Refuse_text, "37ff");
    ("utf8_t21_3", `Refuse_text, "ff");
    ("utf8_t21_2", `Refuse_text, "fe");
    ("utf8_t21_1", `Refuse_text, "81");
    ("utf8_t19_6", `Refuse_text, "313233ef80f0");
    ("utf8_t19_5", `Refuse_text, "313233ef80");
    ("utf8_t19_4", `Refuse_text, "fdbfbfbfbf");
    ("utf8_t19_3", `Refuse_text, "fbbfbfbf");
    ("utf8_t19_2", `Refuse_text, "f7bfbf");
    ("utf8_t19_1", `Refuse_text, "efbf");
    ("utf8_t19_0", `Refuse_text, "df");
    ("utf8_t18_4", `Refuse_text, "fc80808080");
    ("utf8_t18_3", `Refuse_text, "f8808080");
    ("utf8_t18_2", `Refuse_text, "f08080");
    ("utf8_t18_1", `Refuse_text, "e080");
    ("utf8_t18_0", `Refuse_text, "c0");
    ("utf8_t17_1", `Refuse_text, "fd20");
    ("utf8_t17_0", `Refuse_text, "fc20");
    ("utf8_t16_3", `Refuse_text, "fb20");
    ("utf8_t16_2", `Refuse_text, "fa20");
    ("utf8_t16_1", `Refuse_text, "f920");
    ("utf8_t16_0", `Refuse_text, "f820");
    ("utf8_t15_3", `Refuse_text, "f620f720");
    ("utf8_t15_2", `Refuse_text, "f420f520");
    ("utf8_t15_1", `Refuse_text, "f220f320");
    ("utf8_t15_0", `Refuse_text, "f020f120");
    ("utf8_t14_5_1", `Refuse_text, "e180e2f09192f1bf41");
    ("utf8_t14_4_2", `Refuse_text, "f4919293ff4180bf42");
    ("utf8_t14_4_1", `Refuse_text, "eda080edbfbfedaf41");
    ("utf8_t14_4_0", `Refuse_text, "c0afe080bff0818241");
    ("utf8_t14_3", `Refuse_text, "ec20ed20ee20ef20");
    ("utf8_t14_2", `Refuse_text, "e820e920ea20eb20");
    ("utf8_t14_1", `Refuse_text, "e420e520e620e720");
    ("utf8_t14_0", `Refuse_text, "e020e120e220e320");
    ("utf8_t13_7", `Refuse_text, "dc20dd20de20df20");
    ("utf8_t13_6", `Refuse_text, "d820d920da20db20");
    ("utf8_t13_5", `Refuse_text, "d420d520d620d720");
    ("utf8_t13_4", `Refuse_text, "d020d120d220d320");
    ("utf8_t13_3", `Refuse_text, "cc20cd20ce20cf20");
    ("utf8_t13_2", `Refuse_text, "c820c920ca20cb20");
    ("utf8_t13_1", `Refuse_text, "c420c520c620c720");
    ("utf8_t13_0", `Refuse_text, "c020c120c220c320");
    ("utf8_t12_7", `Refuse_text, "b8b9babbbcbdbebf");
    ("utf8_t12_6", `Refuse_text, "b0b1b2b3b4b5b6b7");
    ("utf8_t12_5", `Refuse_text, "a8a9aaabacadaeaf");
    ("utf8_t12_4", `Refuse_text, "a0a1a2a3a4a5a6a7");
    ("utf8_t12_3", `Refuse_text, "98999a9b9c9d9e9f");
    ("utf8_t12_2", `Refuse_text, "9091929394959697");
    ("utf8_t12_1", `Refuse_text, "88898a8b8c8d8e8f");
    ("utf8_t12_0", `Refuse_text, "8081828384858687");
    ("utf8_t11_6", `Refuse_text, "80bf80bf80bf");
    ("utf8_t11_5", `Refuse_text, "80bf80bf80");
    ("utf8_t11_4", `Refuse_text, "80bf80bf");
    ("utf8_t11_3", `Refuse_text, "80bf80");
    ("utf8_t11_2", `Refuse_text, "80bf");
    ("utf8_t11_1", `Refuse_text, "bf");
    ("utf8_t6_5", `Refuse_text, "f7bfbfbfbfbfbf");
    ("utf8_t6_4", `Refuse_text, "f7bfbfbfbfbf");
    ("utf8_t6_3", `Refuse_text, "fc8480808080");
    ("utf8_t6_2", `Refuse_text, "f7bfbfbfbf");
    ("utf8_t6_1", `Refuse_text, "f888808080");
    ("utf8_t6_0_1", `Refuse_text, "f4908080");
    ("utf8_t6_0", `Refuse_text, "f7bfbfbf");
    ("utf8_t22_7", `Accept, "63e0a080");
    ("utf8_t22_1", `Accept, "612f");
    ("utf8_t10_2", `Accept, "63efbfbd");
    ("utf8_t10_1", `Accept, "63ee8080");
    ("utf8_t8_1", `Accept, "62dfbf");
    ("utf8_t8_0", `Accept, "617f");
    ("utf8_t7_3", `Accept, "62c282");
    ("utf8_t7_2", `Accept, "62c281");
    ("utf8_t7_1", `Accept, "62c280");
    ("utf8_t5_3", `Accept, "64f0908080");
    ("utf8_t4_0", `Accept, "64f09d929c");
    ("utf8_t3_0", `Accept, "63e28090");
    ("utf8_t2_1_0", `Accept, "62c2a9");
    ("utf8_t1_0_1", `Accept, "6131");
    ( "arr_2200_deep_canonical",
      `Accept,
      "8181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818101"
    );
    ( "arr_2200_deep_truncated_invalid",
      `Refuse,
      "81818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818181"
    );
    ( "arr_int_boundaries_canonical",
      `Accept,
      "982f3b7fffffffffffffff3b00000001000000003affffffff3afffffffe3afffffffd3a7fffffff3a7ffffffe3a000100013a0001000039ffff39fffe39fffd39010038ff38fe38fd3818373620000001161718181819181a18fe18ff19010019010119fffe19ffff1a000100001a000100011a000100021a7fffffff1a7fffffff1a800000001a800000011afffffffe1affffffff1b00000001000000001b00000001000000011b7fffffffffffffff1bffffffffffffffff"
    );
    ("arr_empties_canonical", `Accept, "83008084808100a0a301a002a00380");
    ( "map_qcbor_complex_nondet",
      `Refuse,
      "a36d666972737420696e7465676572182a77616e206172726179206f662074776f20737472696e67738267737472696e673167737472696e67326c6d617020696e2061206d6170a467627974657320314478787878676279746573203244797979796b616e6f7468657220696e74186266746578742032781e6c6965732c2064616d6e206c69657320616e642073746174697374696373"
    );
    ("uint_uint8_max_canonical", `Accept, "18ff");
    ("uint_256_canonical", `Accept, "190100");
    ("uint_uint16_max_canonical", `Accept, "19ffff");
    ("uint_65536_canonical", `Accept, "1a00010000");
    ("uint_uint32_max_canonical", `Accept, "1affffffff");
    ("uint_uint64_max_minus_one_canonical", `Accept, "1bfffffffffffffffe");
    ("uint_uint64_max_canonical", `Accept, "1bffffffffffffffff");
    ("uint_24_two_byte_nondet", `Refuse, "190018");
    ("uint_24_four_byte_nondet", `Refuse, "1a00000018");
    ("uint_24_eight_byte_nondet", `Refuse, "1b0000000000000018");
    ("uint_uint8_max_two_byte_nondet", `Refuse, "1900ff");
    ("uint_uint16_max_four_byte_nondet", `Refuse, "1a0000ffff");
    ("neg_minus_256_canonical", `Accept, "38ff");
    ("neg_minus_257_canonical", `Accept, "390100");
    ("neg_minus_65536_canonical", `Accept, "39ffff");
    ("neg_minus_65537_canonical", `Accept, "3a00010000");
    ("neg_minus_2pow32_canonical", `Accept, "3affffffff");
    ("neg_minus_2pow32_minus_one_canonical", `Accept, "3b0000000100000000");
    ("neg_min_canonical", `Accept, "3bffffffffffffffff");
    ("neg_minus_one_two_byte_nondet", `Refuse, "390000");
    ( "bstr_23_canonical",
      `Accept,
      "57000102030405060708090a0b0c0d0e0f10111213141516" );
    ( "bstr_24_canonical",
      `Accept,
      "5818000102030405060708090a0b0c0d0e0f1011121314151617" );
    ( "bstr_255_canonical",
      `Accept,
      "58ff000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfe"
    );
    ( "bstr_256_canonical",
      `Accept,
      "590100000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
    );
    ("bstr_short_two_byte_nondet", `Refuse, "590004deadbeef");
    ("bstr_short_eight_byte_nondet", `Refuse, "5b0000000000000004deadbeef");
    ("bstr_oversized_invalid", `Refuse, "59ffff0001");
    ( "tstr_23_canonical",
      `Accept,
      "776161616161616161616161616161616161616161616161" );
    ( "tstr_24_canonical",
      `Accept,
      "7818616161616161616161616161616161616161616161616161" );
    ( "tstr_255_canonical",
      `Accept,
      "78ff616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161"
    );
    ( "tstr_256_canonical",
      `Accept,
      "79010061616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161"
    );
    ("tstr_a_eight_byte_nondet", `Refuse, "7b000000000000000161");
    ("tstr_oversized_invalid", `Refuse, "79ffff6162");
    ( "arr_23_canonical",
      `Accept,
      "97000102030405060708090a0b0c0d0e0f10111213141516" );
    ( "arr_24_canonical",
      `Accept,
      "9818000102030405060708090a0b0c0d0e0f1011121314151617" );
    ("arr_three_one_byte_nondet", `Refuse, "9803010203");
    ("arr_three_two_byte_nondet", `Refuse, "990003010203");
    ("arr_empty_eight_byte_nondet", `Refuse, "9b0000000000000000");
    ("map_two_one_byte_nondet", `Refuse, "b80201010202");
    ("map_mixed_key_types_canonical", `Accept, "a20100616101");
    ("tag_short_canonical", `Accept, "c600");
    ("tag_short_last_canonical", `Accept, "d300");
    ("tag_one_byte_first_canonical", `Accept, "d86300");
    ("tag_one_byte_last_canonical", `Accept, "d8c800");
    ("tag_two_byte_first_canonical", `Accept, "d9010100");
    ("tag_two_byte_last_canonical", `Accept, "d9ffff00");
    ("tag_four_byte_first_canonical", `Accept, "da0001000000");
    ("tag_four_byte_last_canonical", `Accept, "daffffffff00");
    ("tag_eight_byte_first_canonical", `Accept, "db000000010000000000");
    ("tag_max_canonical", `Accept, "dbffffffffffffffff00");
    ("tag_nested_canonical", `Accept, "d904d2d9162e01");
    ("tag_array_payload_canonical", `Accept, "d86383010203");
    ("tag_inner_nondet", `Refuse, "d903e8190018");
    ("simple_zero_canonical", `Accept, "e0");
    ("simple_19_canonical", `Accept, "f3");
    ("simple_32_canonical", `Accept, "f820");
    ("simple_99_canonical", `Accept, "f863");
    ("simple_254_canonical", `Accept, "f8fe");
    ("simple_255_canonical", `Accept, "f8ff");
    ("simple_25_invalid", `Refuse, "f819");
    ("simple_26_invalid", `Refuse, "f81a");
    ("simple_27_invalid", `Refuse, "f81b");
    ("simple_28_invalid", `Refuse, "f81c");
    ("simple_29_invalid", `Refuse, "f81d");
    ("simple_30_invalid", `Refuse, "f81e");
    ("simple_31_invalid", `Refuse, "f81f");
    ("empty_buffer_invalid", `Refuse, "");
    ("trunc_19_invalid", `Refuse, "1900");
    ("trunc_1a_invalid", `Refuse, "1a000000");
    ("trunc_1b_invalid", `Refuse, "1b00000000000000");
    ("trailing_bytes_canonical", `Prefix 1, "00ff");
    ("break_stop_alone_invalid", `Refuse, "ff");
    ("indef_bstr_invalid", `Refuse, "5f40ff");
    ("indef_tstr_invalid", `Refuse, "7f60ff");
    ("indef_arr_zero_invalid", `Refuse, "9fff");
    ("indef_arr_multi_invalid", `Refuse, "9f010203ff");
    ("indef_map_invalid", `Refuse, "bf0102ff");
    ("reserved_uint_1c_invalid", `Refuse, "1c");
    ("reserved_uint_1d_invalid", `Refuse, "1d");
    ("reserved_uint_1e_invalid", `Refuse, "1e");
    ("reserved_negint_3c_invalid", `Refuse, "3c");
    ("reserved_arr_9c_invalid", `Refuse, "9c");
    ("reserved_map_bc_invalid", `Refuse, "bc");
    ("reserved_tag_dc_invalid", `Refuse, "dc");
    ("arr_wide_deep_canonical", `Accept, "84830102038304050683070809830a0b0c");
    ( "arr_25_iter_canonical",
      `Accept,
      "98190102030405060708090a0b0c0d0e0f101112131415161718181819" );
    ("arr_nested_iter_canonical", `Accept, "8301820203820405");
    ( "arr_deeply_nested_iter_canonical",
      `Accept,
      "81818181818181818181818181818181818181818181818181818181818101" );
    ("arr_empties_iter_canonical", `Accept, "83008084808100a0a301a002a00380");
    ( "map_five_iter_canonical",
      `Accept,
      "a56161614161626142616361436164614461656145" );
    ( "map_deeply_nested_iter_canonical",
      `Accept,
      "a100a100a100a100a100a100a100a100a100a10000" );
    ( "map_nested_keys_iter_canonical",
      `Accept,
      "a282a2a2a20000010100a2020003010100a2a20400050100a2060007010101a2a2a20000010100a2020003010100a2a20400050100a20600070101010082a2a2a20000010100a2020003010100a2a20400050100a2060007010101a2a2a20800090100a20a000b010100a2a20c000d0100a20e000f01010101"
    );
    ("tag_nested_iter_canonical", `Accept, "d904d2d9162e01");
    ("tag_array_payload_iter_canonical", `Accept, "d86383010203");
    ( "arr_wide_deep_iter_canonical",
      `Accept,
      "84830102038304050683070809830a0b0c" );
  ]
