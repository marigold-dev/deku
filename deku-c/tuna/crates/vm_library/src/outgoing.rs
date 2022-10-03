use fnv::FnvHashMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::state::ContractType;

#[derive(Debug, Deserialize, Serialize)]
pub struct SetOwned {
    pub key: String,
    #[serde(serialize_with = "base64ser", deserialize_with = "base64deser")]
    pub value: ContractType,
}
fn base64ser<S>(t: &ContractType, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let ok = base64::encode(
        bincode::serialize(&t).map_err(|err| serde::ser::Error::custom(err.to_string()))?,
    );
    serializer.serialize_str(&ok)
}
fn base64deser<'de, S>(deser: S) -> Result<ContractType, S::Error>
where
    S: Deserializer<'de>,
{
    let s: &str = Deserialize::deserialize(deser)?;
    let ok = base64::decode(s).map_err(|err| serde::de::Error::custom(err.to_string()))?;
    let ok = bincode::deserialize(&ok).map_err(|err| serde::de::Error::custom(err.to_string()))?;
    Ok(ok)
}
#[repr(transparent)]
#[derive(Deserialize, Serialize, Debug)]
pub struct Init(pub FnvHashMap<String, String>);
#[repr(transparent)]
#[derive(Deserialize, Serialize, Debug)]
pub struct InitVec(pub Vec<SetOwned>);
