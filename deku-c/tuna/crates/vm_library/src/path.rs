use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Path {
    Left,
    Right,
}
