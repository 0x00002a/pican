use pican_core::ir::Float;

/// 24-bit floating-point representation used by the PICA200
///
/// This is a container for the floating point representation used
/// by the PICA200 (nintendo 3ds GPU), it is intended as a black-box
/// that can be directly copied to the GPU/stored in shaders
/// and not for performing arithmatic with.
///
/// More info: https://www.3dbrew.org/wiki/GPU/Shader_Instruction_Set#Floating-Point_Behavior
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
#[binrw::binrw]
#[brw(little)]
pub struct Float24([u8; 3]);

impl std::fmt::Debug for Float24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_f32().fmt(f)
    }
}

#[derive(Debug)]
pub enum F32ToF24ConversionError {
    ExponentUnderflow,
    ExponentOverflow,
}

impl std::fmt::Display for F32ToF24ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            F32ToF24ConversionError::ExponentUnderflow => f.write_str("exponent would underflow"),
            F32ToF24ConversionError::ExponentOverflow => f.write_str("exponent would overflow"),
        }
    }
}

#[derive(Debug)]
struct FloatParts {
    sign: u32,
    exp: u32,
    mant: u32,
}
impl std::fmt::Display for FloatParts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{sign}|{exp:b}|{mant:b}",
            sign = self.sign,
            exp = self.exp as u8,
            mant = self.mant
        ))
    }
}

fn split_up_float(v: f32) -> FloatParts {
    let vb = u32::from_le_bytes(v.to_le_bytes());
    // layout is <sign:1><exp:8><mant:23>
    let sign = vb >> 31;
    let exp = (vb << 1) >> 24;
    let mant = (vb << 9) >> 9;
    FloatParts { sign, exp, mant }
}

impl Float24 {
    /// Attempt to convert an f32
    pub fn try_from_f32(v: f32) -> Result<Self, F32ToF24ConversionError> {
        if v == 0.0 {
            return Ok(Self([0, 0, 0]));
        }
        let FloatParts { sign, exp, mant } = split_up_float(v);

        // target layout is <sign:1><exp:7><mant:16>

        // adjust the bias: https://en.wikipedia.org/wiki/Exponent_bias
        // since we only have 7 bits we convert first to the unbiased version
        // then back to the bias version for 7 bits
        let to_exp = (exp as i32) - 127 + 63;
        if to_exp < 0 {
            return Err(F32ToF24ConversionError::ExponentUnderflow);
        } else if exp > 127 {
            return Err(F32ToF24ConversionError::ExponentOverflow);
        }
        // have to strip the bottom 7 bits of this
        let to_mant = mant >> 7;
        assert!(to_mant <= (u16::MAX as u32));

        let packed = (sign << 23) | ((to_exp as u32) << 16) | to_mant;
        let pb = packed.to_le_bytes();

        Ok(Self([pb[1], pb[2], pb[3]]))
    }
    /// Convert to the bit representation
    ///
    /// Bits are always little-endian
    pub fn to_bits(self) -> u32 {
        u32::from_le_bytes([0, self.0[0], self.0[1], self.0[2]])
    }
    pub fn to_f32(self) -> f32 {
        let packed = self.to_bits();
        let sign = packed >> 23;
        let exp = (packed << 9) >> 24;
        // have to pad this out for signed math from 7bit to 8bit
        let padded_exp = ((exp >> 7) << 7) | ((exp << 1) >> 2);
        let to_exp = (padded_exp as i32) - 63 + 127;
        let mant = (packed << 16) >> 16;
        f32::from_le_bytes(((sign << 31) | ((to_exp as u32) << 23) | mant).to_le_bytes())
    }
}
impl TryFrom<f32> for Float24 {
    type Error = F32ToF24ConversionError;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        Self::try_from_f32(value)
    }
}

impl TryFrom<Float> for Float24 {
    type Error = F32ToF24ConversionError;

    fn try_from(value: Float) -> Result<Self, Self::Error> {
        Self::try_from_f32(value.into_inner())
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use crate::float24::{split_up_float, Float24};

    #[test]
    fn f32_to_f24_preserves_negatives() {
        fn roundtrip(v: f32) -> f32 {
            Float24::try_from_f32(v).unwrap().to_f32()
        }

        #[track_caller]
        fn check_roundtrips(v: f32) {
            assert_eq!(
                v,
                roundtrip(v),
                "\n{:#b} != \n{:#b} intermediate: \n{:#b} parts\n{} vs \n{}",
                v.to_bits(),
                roundtrip(v).to_bits(),
                Float24::try_from_f32(v).unwrap().to_bits(),
                split_up_float(v),
                split_up_float(roundtrip(v)),
            );
        }

        check_roundtrips(0.5);
        check_roundtrips(-1.);
        check_roundtrips(1.);
        check_roundtrips(0.25);
        check_roundtrips(-0.25);
    }
    #[test]
    fn f24_from_zero() {
        assert_matches!(Float24::try_from_f32(0.0), Ok(_));
    }
}
