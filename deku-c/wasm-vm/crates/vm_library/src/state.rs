use fnv::FnvHashMap;
use serde::{Deserialize, Serialize};
use wasmer::Module;

use crate::{
    compile_store,
    contract_address::ContractAddress,
    errors::{vm::VmError, VMResult},
    outgoing::{Init, InitVec, SetOwned},
    path::Path,
};
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ContractType {
    pub self_: ContractAddress,
    pub originated_by: String,
    #[serde(with = "serde_bytes")]
    pub storage: Vec<u8>,
    #[serde(skip_deserializing, skip_serializing)]
    pub module: Option<Box<Module>>,
    #[serde(with = "serde_bytes")]
    pub serialized_module: Vec<u8>,
    #[serde(with = "serde_bytes")]
    pub constants: Vec<u8>,
    pub entrypoints: Option<FnvHashMap<String, Vec<Path>>>,
}
impl ContractType {
    pub fn set_storage(&mut self, s: Vec<u8>) {
        self.storage = s
    }
    pub fn init(&mut self) -> VMResult<()> {
        match self.module {
            None => {
                self.module = Some(Box::from(unsafe {
                    Module::deserialize(&compile_store::new_headless(), &self.serialized_module)
                }?));
                Ok::<(), VmError>(())
            }
            Some(_) => Ok(()),
        }
    }
}
impl PartialEq for ContractType {
    fn eq(&self, other: &Self) -> bool {
        self.self_ == other.self_
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

    pub fn get(&mut self, key: &String) -> Option<ContractType> {
        self.table.remove(key)
    }
    pub fn from_init(&mut self, init: Init) -> VMResult<()> {
        self.table.clear();
        init.0.iter().try_for_each(|(key, value)| {
            let contract_type: ContractType = bincode::deserialize(
                &base64::decode(value).map_err(|err| VmError::DeserializeErr(err.to_string()))?,
            )
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
