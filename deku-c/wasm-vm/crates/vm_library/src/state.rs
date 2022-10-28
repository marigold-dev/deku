use crate::managed::value::FromOcamlV;
use crate::{
    compile_store,
    contract_address::ContractAddress,
    errors::{vm::VmError, VMResult},
    managed::value::Value,
    outgoing::{Init, InitVec, SetOwned},
    path::Path,
};
use fnv::FnvHashMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use wasmer::Module;
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LigoCode {
    type_: String,
    code: String,
}
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LigoContractState {
    pub self_: ContractAddress,
    pub originated_by: String,
    pub storage: Box<FromOcamlV>,
    #[serde(with = "serde_bytes")]
    pub serialized_module: Vec<u8>,
    pub constants: Vec<(u32, Value)>,
    pub entrypoints: Option<FnvHashMap<String, Vec<Path>>>,
    pub source: Option<LigoCode>,
    #[serde(skip_deserializing, skip_serializing)]
    pub module: Option<Box<Module>>,
}
fn json_ser<S>(t: &Value, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let val = serde_json::to_string(&FromOcamlV(t.clone()))
        .map_err(|err| serde::ser::Error::custom(err.to_string()))?;

    serializer.serialize_str(&val)
}
fn json_deser<'de, S>(deser: S) -> Result<Box<Value>, S::Error>
where
    S: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deser)?;
    let ok: FromOcamlV =
        serde_json::from_str(&s).map_err(|err| serde::de::Error::custom(err.to_string()))?;
    Ok(Box::new(ok.0))
}
#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum ContractType {
    LigoContract(LigoContractState),
}
impl ContractType {
    pub fn set_storage(&mut self, s: Box<FromOcamlV>) {
        match self {
            Self::LigoContract(l) => l.storage = s,
        }
    }
    pub fn module(&self) -> &Option<Box<Module>> {
        match self {
            Self::LigoContract(l) => &l.module,
        }
    }
    pub fn constants(&self) -> &[(u32, Value)] {
        match self {
            Self::LigoContract(l) => &l.constants,
        }
    }
    pub fn entrypoints(&self) -> &Option<FnvHashMap<String, Vec<Path>>> {
        match self {
            Self::LigoContract(l) => &l.entrypoints,
        }
    }
    pub fn storage(&self) -> &FromOcamlV {
        match self {
            Self::LigoContract(l) => &l.storage,
        }
    }
    pub fn init(&mut self) -> VMResult<()> {
        match self {
            Self::LigoContract(s) => match s.module {
                None => {
                    s.module = Some(Box::from(unsafe {
                        Module::deserialize(&compile_store::new_headless(), &s.serialized_module)
                    }?));
                    Ok::<(), VmError>(())
                }
                Some(_) => Ok(()),
            },
        }
    }
}
impl PartialEq for ContractType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::LigoContract(s), Self::LigoContract(s2)) => s.self_ == s2.self_,
        }
    }
}
impl Eq for ContractType {
    fn assert_receiver_is_total_eq(&self) {}
}

#[derive(PartialEq, Eq, Debug)]
pub struct State {
    pub table: FnvHashMap<String, ContractType>,
}
impl Default for State {
    fn default() -> Self {
        Self {
            table: FnvHashMap::with_capacity_and_hasher(1000, Default::default()),
        }
    }
}

impl State {
    pub fn set(&mut self, key: String, value: ContractType) -> Option<ContractType> {
        self.table.insert(key, value)
    }
    pub fn reset(&mut self, key: String, value: Option<FromOcamlV>) {
        if value.is_none() {
            self.table.remove(&key);
            return;
        }
        if let Some(x) = self.table.get_mut(&key) {
            x.set_storage(Box::from(value.unwrap()))
        }
    }
    pub fn get(&mut self, key: &String) -> Option<ContractType> {
        self.table.remove(key)
    }
    pub fn from_init(&mut self, init: Init) -> VMResult<()> {
        self.table.clear();
        init.0.iter().try_for_each(|(key, value)| {
            let contract_type: ContractType = serde_json::from_str(value)
                .map_err(|err| VmError::DeserializeErr(err.to_string()))?;
            self.table.insert(key.clone(), contract_type);
            Ok(())
        })
    }
    pub fn to_init(&self) -> VMResult<InitVec> {
        let acc = self
            .table
            .iter()
            .map(|(contract_address, contract_type)| SetOwned {
                key: contract_address.clone(),
                value: contract_type.clone(),
            })
            .collect();
        Ok(InitVec(acc))
    }
}
