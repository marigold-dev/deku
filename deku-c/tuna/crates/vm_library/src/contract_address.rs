use blake2::digest::consts::U20;
use blake2::Digest;
type Blake2b160 = blake2::Blake2b<U20>;
use serde::{de::Visitor, Deserialize, Serialize};
use sha2::{Digest as D, Sha256};

fn checksum(mut s: Vec<u8>) -> Vec<u8> {
    let one = Sha256::digest(&s);
    s.append(&mut Sha256::digest(&one)[0..4].to_vec());
    s
}

fn encode(s: &[u8]) -> String {
    let mut prefix = vec![1, 146, 6];
    prefix.append(&mut s.to_vec());
    bs58::encode(checksum(prefix))
        .with_alphabet(bs58::Alphabet::BITCOIN)
        .into_string()
}
pub fn decode(s: &[u8]) -> Result<Vec<u8>, bs58::decode::Error> {
    let s = bs58::decode(&s)
        .with_alphabet(bs58::Alphabet::BITCOIN)
        .into_vec()?;
    let s = &s[3..s.len() - 4];
    Ok(s.to_vec())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ContractAddress {
    pub address: String,
    pub entrypoint: Option<String>,
}
struct ContractVisitor;
impl<'de> Visitor<'de> for ContractVisitor {
    type Value = ContractAddress;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Expected a valid Value")
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match v.split_once('%') {
            Some((addr, entrypoint)) => Ok(ContractAddress {
                address: addr.to_owned(),
                entrypoint: Some(entrypoint.to_owned()),
            }),
            None => Ok(ContractAddress {
                address: v.to_owned(),
                entrypoint: None,
            }),
        }
    }
}
impl<'de> Deserialize<'de> for ContractAddress {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(ContractVisitor)
    }
}
impl Serialize for ContractAddress {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match &self.entrypoint {
            None => serializer.serialize_str(&self.address),
            Some(entry) => serializer.serialize_str(&format!("{}%{}", self.address, entry)),
        }
    }
}
impl ContractAddress {
    pub fn new(s: &[u8]) -> Self {
        Self {
            address: encode(&Blake2b160::digest(s)),
            entrypoint: None,
        }
    }
}
