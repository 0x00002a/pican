use binrw::binrw;
use serde::{Deserialize, Serialize};

/// Possible output properties for a shader program
///
/// See for more https://github.com/devkitPro/picasso/blob/master/Manual.md#out
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[binrw]
pub enum OutputProperty {
    #[brw(magic = 0u8)]
    Position,
    #[brw(magic = 1u8)]
    NormalQuat,
    #[brw(magic = 2u8)]
    Color,
    #[brw(magic = 3u8)]
    TexCoord0,
    #[brw(magic = 4u8)]
    TexCoord0W,
    #[brw(magic = 5u8)]
    TexCoord1,
    #[brw(magic = 6u8)]
    TexCoord2,
    #[brw(magic = 7u8)]
    Dummy,
    #[brw(magic = 8u8)]
    View,
}
