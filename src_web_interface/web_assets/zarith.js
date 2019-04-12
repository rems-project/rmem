/* Licensed under the Apache License 2.0. */
/* From https://github.com/cpitclaudel/fstar.js/blob/master/src/js/zarith.js */

/* global bigInt caml_to_js_string caml_js_to_string caml_failwith */
/* eslint-disable no-unused-vars */

//Provides: ml_z_init const
function ml_z_init() {}

//Provides: ml_z_install_frametable const
function ml_z_install_frametable() {}

// external neg: t -> t = neg@ASM
//Provides: ml_z_neg const
//Requires: bigInt
function ml_z_neg(z1) {
    return bigInt(z1).negate();
}

// external add: t -> t -> t = add@ASM
//Provides: ml_z_add const
//Requires: bigInt
function ml_z_add(z1, z2) {
    return bigInt(z1).add(bigInt(z2));
}

// external sub: t -> t -> t = sub@ASM
//Provides: ml_z_sub const
//Requires: bigInt
function ml_z_sub(z1, z2) {
    return bigInt(z1).subtract(bigInt(z2));
}

// external mul: t -> t -> t = mul@ASM
//Provides: ml_z_mul const
//Requires: bigInt
function ml_z_mul(z1, z2) {
    return bigInt(z1).multiply(bigInt(z2));
}

// external div: t -> t -> t = div@ASM
//Provides: ml_z_div const
//Requires: bigInt
function ml_z_div(z1, z2) {
    return bigInt(z1).divide(bigInt(z2)); // FIXME
}

// external cdiv: t -> t -> t = "ml_z_cdiv"
//Provides: ml_z_cdiv const
//Requires: bigInt
function ml_z_cdiv(z1, z2) {
    return bigInt(z1).divide(bigInt(z2)); // FIXME
}

// external fdiv: t -> t -> t = "ml_z_fdiv"
//Provides: ml_z_fdiv const
//Requires: bigInt
function ml_z_fdiv(z1, z2) {
    return bigInt(z1).divide(bigInt(z2)); // FIXME
}

// external rem: t -> t -> t = rem@ASM
//Provides: ml_z_rem const
//Requires: bigInt
function ml_z_rem(z1, z2) {
    return bigInt(z1).mod(bigInt(z2)); // FIXME
}

// external div_rem: t -> t -> (t * t) = "ml_z_div_rem"
// Should provide ml_z_div_rem const
function ml_z_div_rem(z1, z2) {
}

// external succ: t -> t = succ@ASM
//Provides: ml_z_succ const
//Requires: bigInt
function ml_z_succ(z1) {
    return bigInt(z1).next();
}

// external pred: t -> t = pred@ASM
//Provides: ml_z_pred const
//Requires: bigInt
function ml_z_pred(z1) {
    return bigInt(z1).prev();
}

// external abs: t -> t = abs@ASM
//Provides: ml_z_abs const
//Requires: bigInt
function ml_z_abs(z1) {
    return bigInt(z1).abs();
}

// external logand: t -> t -> t = logand@ASM
//Provides: ml_z_logand const
//Requires: bigInt
function ml_z_logand(z1, z2) {
    return bigInt(z1).and(bigInt(z2));
}

// external logor: t -> t -> t = logor@ASM
//Provides: ml_z_logor const
//Requires: bigInt
function ml_z_logor(z1, z2) {
    return bigInt(z1).or(bigInt(z2));
}

// external logxor: t -> t -> t = logxor@ASM
//Provides: ml_z_logxor const
//Requires: bigInt
function ml_z_logxor(z1, z2) {
    return bigInt(z1).xor(bigInt(z2));
}

// external lognot: t -> t = lognot@ASM
//Provides: ml_z_lognot const
//Requires: bigInt
function ml_z_lognot(z1) {
    return bigInt(z1).lognot();
}

// external shift_left: t -> int -> t = shift_left@ASM
//Provides: ml_z_shift_left const
//Requires: bigInt
function ml_z_shift_left(z1, amt) {
    return bigInt(z1).shiftLeft(amt);
}

// external shift_right: t -> int -> t = shift_right@ASM
//Provides: ml_z_shift_right const
//Requires: bigInt
function ml_z_shift_right(z1, amt) {
    return bigInt(z1).shiftRight(amt);
}

// external shift_right_trunc: t -> int -> t = shift_right_trunc@ASM
// Should provide ml_z_shift_right_trunc const
function ml_z_shift_right_trunc(z1, z2) {

}

// external of_int: int -> t = "ml_z_of_int" @NOALLOC
//Provides: ml_z_of_int const
//Requires: bigInt
function ml_z_of_int(i) {
    return bigInt(i);
}

// external of_int32: int32 -> t = "ml_z_of_int32"
//Provides: ml_z_of_int32 const
//Requires: bigInt
function ml_z_of_int32(i32) {
    return bigInt(i32);
}

// external of_int64: int64 -> t = "ml_z_of_int64"
//Provides: ml_z_of_int64 const
//Requires: bigInt
function ml_z_of_int64(i64) {
    return bigInt(i64[3]).shiftLeft(48).add(bigInt(i64[2]).shiftLeft(24)).add(bigInt(i64[1]));
}

// external of_nativeint: nativeint -> t = "ml_z_of_nativeint"
// Should provide ml_z_of_nativeint const
function ml_z_of_nativeint(z1, z2) {

}

// external of_float: float -> t = "ml_z_of_float"
//Provides: ml_z_of_float const
//Requires: bigInt
function ml_z_of_float(z1) {
    return bigInt(Math.floor(z1));
}

// FIXME all conversion functions should be checked (toJSNumber is likely wrong for int64 and int32)

// external to_int: t -> int = "ml_z_to_int"
//Provides: ml_z_to_int const
//Requires: bigInt
function ml_z_to_int(z1) {
    return bigInt(z1).toJSNumber();
}

// external to_int32: t -> int32 = "ml_z_to_int32"
//Provides: ml_z_to_int32 const
//Requires: bigInt
function ml_z_to_int32(z1) {
    return bigInt(z1).toJSNumber();
}

// external to_int64: t -> int64 = "ml_z_to_int64"
//Provides: ml_z_to_int64 const
//Requires: bigInt
function ml_z_to_int64(z1) {
    var z = bigInt(z1);
    
    return [255, z.and(0xffffff), z.shiftRight(24).and(bigInt(0xffffff)), z.shiftRight(48).and(bigInt(0xffff))];
}

// external to_nativeint: t -> nativeint = "ml_z_to_nativeint"
//Provides: ml_z_to_nativeint const
//Requires: bigInt
function ml_z_to_nativeint(z1) {
    return bigInt(z1).toJSNumber();
}

// external to_float: t -> nativeint = "ml_z_to_float"
//Provides: ml_z_to_float const
//Requires: bigInt
function ml_z_to_float(z1) {
    return bigInt(z1).toJSNumber();
}

// external format: string -> t -> string = "ml_z_format"
//Provides: ml_z_format const
//Requires: bigInt
//Requires: caml_to_js_string, caml_js_to_string, caml_failwith
function ml_z_format(fmt, z1) {
    fmt = caml_to_js_string(fmt);
    if (fmt == "%d") {
        return caml_js_to_string(bigInt(z1).toString());
    } else {
        caml_failwith("Unsupported format '" + fmt + "'");
    }
}

//Provides: ml_z_of_js_string_base const
//Requires: bigInt, caml_to_js_string, caml_failwith
function ml_z_of_js_string_base(base, s) {
    if (base == 0) { // https://github.com/ocaml/Zarith/blob/b8dbaf48a7927061df699ad7ce642bb4f1fe5308/caml_z.c#L600
        base = 10;

        if (s[0] == '0') {
            if (s.length == 1) {
                return bigInt(0);
            } else {
                var bc = s[1];
                if (bc == 'o' || bc == 'O') {
                    base = 8;
                } else if (bc == 'x' || bc == 'X') {
                    base = 16;
                } else if (bc == 'b' || bc == 'B') {
                    base = 2;
                } else {
                    caml_failwith("Z.of_substring_base: invalid digit");
                }

                s = s.substring(2);
            }
        }
    }
    return bigInt(s, base);
}

// external of_string_base: int -> string -> t = "ml_z_of_string_base"
//Provides: ml_z_of_string_base const
//Requires: caml_to_js_string, ml_z_of_js_string_base
function ml_z_of_string_base(base, s) {
    return ml_z_of_js_string_base(base, caml_to_js_string(s));
}

// external of_substring_base: int -> string -> pos:int -> len:int -> t = "ml_z_of_substring_base"
//Provides: ml_z_of_substring_base const
//Requires: caml_to_js_string, ml_z_of_js_string_base
function ml_z_of_substring_base(base, s, pos, len) {
    return ml_z_of_js_string_base(base, caml_to_js_string(s).substring(pos, pos + len));
}

// external compare: t -> t -> int = "ml_z_compare" @NOALLOC
//Provides: ml_z_compare const
//Requires: bigInt
function ml_z_compare(z1, z2) {
    return bigInt(z1).compare(bigInt(z2));
}

// external equal: t -> t -> bool = "ml_z_equal" @NOALLOC
//Provides: ml_z_equal const
//Requires: bigInt
function ml_z_equal(z1, z2) {
    return bigInt(z1).equals(bigInt(z2)) ? 1 : 0;
}

// external sign: t -> int = "ml_z_sign" @NOALLOC
//Provides: ml_z_sign const
//Requires: bigInt
function ml_z_sign(z1) {
    return bigInt(z1).compare(bigInt.zero);
}

// external gcd: t -> t -> t = "ml_z_gcd"
//Provides: ml_z_gcd const
//Requires: bigInt
function ml_z_gcd(z1, z2) {
    return bigInt.gcd(z1, z2);
}

// external gcdext_intern: t -> t -> (t * t * bool) = "ml_z_gcdext_intern"
// Should provide ml_z_gcdext_intern const
function ml_z_gcdext_intern(z1, z2) {

}

// external sqrt: t -> t = "ml_z_sqrt"
// Should provide ml_z_sqrt const
function ml_z_sqrt(z1, z2) {

}

// external sqrt_rem: t -> (t * t) = "ml_z_sqrt_rem"
// Should provide ml_z_sqrt_rem const
function ml_z_sqrt_rem(z1, z2) {

}

// external numbits: t -> int = "ml_z_numbits" @NOALLOC
//Provides: ml_z_numbits const
//Requires: bigInt
function ml_z_numbits(z1) {
    z1 = bigInt(z1).abs();
    var n = 0;
    var upperBound = bigInt.one;
    while (upperBound.leq(z1)) {
        n += 1;
        upperBound = upperBound.multiply(2);
    }
    return n; // 2^{n-1} <= |x| < 2^n
}

// external trailing_zeros: t -> int = "ml_z_trailing_zeros" @NOALLOC
// Should provide ml_z_trailing_zeros const
function ml_z_trailing_zeros(z1, z2) {

}

// external popcount: t -> int = "ml_z_popcount"
// Should provide ml_z_popcount const
function ml_z_popcount(z1, z2) {

}

// external hamdist: t -> t -> int = "ml_z_hamdist"
// Should provide ml_z_hamdist const
function ml_z_hamdist(z1, z2) {

}

// external size: t -> int = "ml_z_size" @NOALLOC
// Should provide ml_z_size const
function ml_z_size(z1, z2) {

}

// external fits_int: t -> bool = "ml_z_fits_int" @NOALLOC
// Should provide ml_z_fits_int const
function ml_z_fits_int(z1, z2) {

}

// external fits_int32: t -> bool = "ml_z_fits_int32" @NOALLOC
// Should provide ml_z_fits_int32 const
function ml_z_fits_int32(z1, z2) {

}

// external fits_int64: t -> bool = "ml_z_fits_int64" @NOALLOC
// Should provide ml_z_fits_int64 const
function ml_z_fits_int64(z1, z2) {

}

// external fits_nativeint: t -> bool = "ml_z_fits_nativeint" @NOALLOC
// Should provide ml_z_fits_nativeint const
function ml_z_fits_nativeint(z1, z2) {

}

// external extract: t -> int -> int -> t = "ml_z_extract"
//Provides: ml_z_extract const
//Requires: bigInt
function ml_z_extract(z, from_end, length) {
    z = bigInt(z);
    var obj = z.toArray(2);
    if (obj.isNegative) { throw new Exception("negative numbers not supported yet in ml_z_extract"); }
    var start = obj.value.length - from_end - length;
    if (start < 0) start = 0;
    var end = obj.value.length - from_end;
    if (end < 0) end = 0;
    return bigInt.fromArray(obj.value.slice(start, end), 2, false);
}

// external powm: t -> t -> t -> t = "ml_z_powm"
//Provides: ml_z_powm const
//Requires: bigInt
function ml_z_powm(z1, z2, z3) {
    return bigInt(z1).modPow(bigInt(z2), bigInt(z3));
}

// external pow: t -> int -> t = "ml_z_pow"
//Provides: ml_z_pow const
//Requires: bigInt
function ml_z_pow(z1, i1) {
    return bigInt(z1).pow(bigInt(i1));
}

// external powm_sec: t -> t -> t -> t = "ml_z_powm_sec"
// Should provide ml_z_powm_sec const
function ml_z_powm_sec(z1, z2) {

}

// external divexact: t -> t -> t = divexact@ASM
// Should provide ml_z_divexact const
function ml_z_divexact(z1, z2) {

}

// external root: t -> int -> t = "ml_z_root"
// Should provide ml_z_root const
function ml_z_root(z1, z2) {

}

// external invert: t -> t -> t = "ml_z_invert"
// Should provide ml_z_invert const
function ml_z_invert(z1, z2) {

}

// external perfect_power: t -> bool = "ml_z_perfect_power"
// Should provide ml_z_perfect_power const
function ml_z_perfect_power(z1, z2) {

}

// external perfect_square: t -> bool = "ml_z_perfect_square"
// Should provide ml_z_perfect_square const
function ml_z_perfect_square(z1, z2) {

}

// external probab_prime: t -> int -> int = "ml_z_probab_prime"
// Should provide ml_z_probab_prime const
function ml_z_probab_prime(z1, z2) {

}

// external nextprime: t -> t = "ml_z_nextprime"
// Should provide ml_z_nextprime const
function ml_z_nextprime(z1, z2) {

}

// external hash: t -> int = "ml_z_hash" @NOALLOC
// Should provide ml_z_hash const
function ml_z_hash(z1, z2) {

}

// external to_bits: t -> string = "ml_z_to_bits"
// Should provide ml_z_to_bits const
function ml_z_to_bits(z1, z2) {

}

// external of_bits: string -> t = "ml_z_of_bits"
// Should provide ml_z_of_bits const
function ml_z_of_bits(z1, z2) {

}
