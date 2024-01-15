use serde::{Deserialize, Serialize};

/// Possible output properties for a shader program
///
/// See for more https://github.com/devkitPro/picasso/blob/master/Manual.md#out
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum OutputProperty {
    Position,
    NormalQuat,
    Color,
    TexCoord0,
    TexCoord0W,
    TexCoord1,
    TexCoord2,
    View,
    Dummy,
}
