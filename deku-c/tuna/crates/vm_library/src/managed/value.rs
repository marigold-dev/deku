use crate::{
    arena::{ARENA, CONSUMEDTICKETS, INVERSETICKETS, TICKETS},
    ticket_table::Ticket,
};
use im_rc::{OrdMap, OrdSet, Vector};
use serde::{de::Visitor, ser::SerializeTuple, Deserialize, Serialize};
use slotmap::DefaultKey;
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub enum Union {
    Left(DefaultKey),
    Right(DefaultKey),
}
unsafe impl Sync for Union {}
unsafe impl Send for Union {}
impl Serialize for Union {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use Union::*;
        let for_removal = unsafe { &ARENA };

        match self {
            Right(x) => {
                let mut seq = serializer.serialize_tuple(2)?;

                seq.serialize_element("Right")?;
                let value1 = for_removal
                    .get(*x)
                    .map_or_else(|| Err(serde::ser::Error::custom(&"error serializing")), Ok)?;
                seq.serialize_element(value1)?;
                seq.end()
            }
            Left(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Left")?;
                let value1 = for_removal
                    .get(*x)
                    .map_or_else(|| Err(serde::ser::Error::custom(&"error serializing")), Ok)?;

                seq.serialize_element(value1)?;
                seq.end()
            }
        }
    }
}
struct UnionVisitor;
impl<'de> Visitor<'de> for UnionVisitor {
    type Value = Union;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Expected a Union")
    }
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let arena = unsafe { &mut ARENA };

        seq.next_element::<&str>()?.map_or_else(
            || {
                Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &"value",
                ))
            },
            |x| match x {
                "Left" => {
                    let elem = seq.next_element::<Value>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| {
                            let inserted = arena.insert(x);
                            Ok(Union::Left(inserted))
                        },
                    )
                }
                "Right" => {
                    let elem = seq.next_element::<Value>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| {
                            let inserted = arena.insert(x);
                            Ok(Union::Right(inserted))
                        },
                    )
                }
                _ => Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &self,
                )),
            },
        )
    }
}
impl<'de> Deserialize<'de> for Union {
    fn deserialize<D>(deserializer: D) -> Result<Union, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_tuple(2, UnionVisitor)
    }
}
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Copy, Clone)]
pub enum Tag {
    Bytes,
    String,
}
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum Value {
    Bytes(Vec<u8>),
    String(String),
    Int(rug::Integer),
    Union(Union),
    Pair {
        fst: DefaultKey,
        snd: DefaultKey,
    },
    Bool(bool),
    Map(OrdMap<Value, Value>),
    Set(OrdSet<Value>),
    List(Vector<Value>, Option<Tag>),
    Unit,
    Option(Option<DefaultKey>),
    Ticket(usize),
    Closure {
        opt_arg: Option<DefaultKey>,
        call: i32,
    },
}
impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Closure { opt_arg, call } => {
                let opt_arg = match opt_arg {
                    None => None,
                    Some(x) => {
                        let arena = unsafe { &mut ARENA };
                        let value = arena.get(*x);
                        value.cloned().map(|x| arena.insert(x))
                    }
                };
                Self::Closure {
                    opt_arg,
                    call: *call,
                }
            }
            Self::Bytes(arg0) => Self::Bytes(arg0.clone()),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Int(arg0) => Self::Int(arg0.clone()),
            Self::Union(arg0) => Self::Union(arg0.clone()),
            Self::Pair { fst, snd } => {
                let arena = unsafe { &mut ARENA };
                let fst = arena
                    .get(*fst)
                    .cloned()
                    .map(|x| arena.insert(x))
                    .expect("runtime error");
                let snd = arena
                    .get(*snd)
                    .cloned()
                    .map(|x| arena.insert(x))
                    .expect("runtime error");

                Self::Pair { fst, snd }
            }
            Self::Bool(arg0) => Self::Bool(*arg0),
            Self::Map(arg0) => Self::Map(arg0.clone()),
            Self::Set(arg0) => Self::Set(arg0.clone()),
            Self::List(arg0, tag) => Self::List(arg0.clone(), *tag),
            Self::Unit => Self::Unit,
            Self::Option(arg0) => match arg0 {
                None => Self::Option(None),
                Some(x) => {
                    let arena = unsafe { &mut ARENA };
                    let value = arena.get(*x);
                    let value = value.cloned().map(|x| arena.insert(x));
                    Self::Option(value)
                }
            },
            Self::Ticket(x) => Self::Ticket(*x),
        }
    }
}
unsafe impl Sync for Value {}
unsafe impl Send for Value {}

#[repr(transparent)]
#[derive(Clone)]
pub struct ValueVisitor;
impl<'de> Visitor<'de> for ValueVisitor {
    type Value = Value;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Expected a valid Value")
    }
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let arena = unsafe { &mut ARENA };
        seq.next_element::<&str>()?.map_or_else(
            || {
                Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &"value",
                ))
            },
            |x| match x {
                "Unit" => Ok(Value::Unit),
                "Bool" => {
                    let elem = seq.next_element::<bool>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| Ok(Value::Bool(x)),
                    )
                }
                "Int" => {
                    let elem = seq.next_element::<&str>()?;
                    let elem: Option<rug::Integer> = elem
                        .map(|elem| rug::Integer::from_str_radix(elem, 10))
                        .transpose()
                        .map_or_else(|_| None, |x| x);
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &self,
                            ))
                        },
                        |x| Ok(Value::Int(x)),
                    )
                }
                "Bytes" => {
                    let elem = seq.next_element::<&str>()?;
                    let elem = elem.map(|elem| elem.as_bytes().to_vec());
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &self,
                            ))
                        },
                        |x| Ok(Value::Bytes(x)),
                    )
                }
                "String" => {
                    let elem = seq.next_element::<&str>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &self,
                            ))
                        },
                        |x| Ok(Value::String(x.to_owned())),
                    )
                }
                "Union" => {
                    let elem = seq.next_element::<Union>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"test",
                            ))
                        },
                        |x| Ok(Value::Union(x)),
                    )
                }
                "List" => {
                    let elem = seq.next_element::<Vector<Value>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |x| {
                            let tag = match &x.head() {
                                Some(Value::Bytes(_)) => Some(Tag::Bytes),
                                Some(Value::String(_)) => Some(Tag::String),
                                Some(_) | None => None,
                            };
                            Ok(Value::List(x, tag))
                        },
                    )
                }
                "Ticket" => {
                    let elem = seq.next_element::<Ticket>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |x| {
                            let tickets = unsafe { &mut INVERSETICKETS };
                            let ticket = { tickets.get(&x) }
                                .map_or_else(|| Err(serde::de::Error::custom(&"Value enum")), Ok)?;
                            Ok(Value::Ticket(*ticket))
                        },
                    )
                }
                "Map" => {
                    let elem = seq.next_element::<Vec<(Value, Value)>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |elem| Ok(Value::Map(OrdMap::from(elem))),
                    )
                }
                "Set" => {
                    let elem = seq.next_element::<Vec<Value>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |x| Ok(Value::Set(OrdSet::from(x))),
                    )
                }
                "Pair" => {
                    let elem1 = seq.next_element::<Value>()?;
                    let elem1 = elem1.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &self,
                            ))
                        },
                        |v| Ok(arena.insert(v)),
                    )?;
                    let elem2 = seq.next_element::<Value>()?;
                    let elem2 = elem2.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &self,
                            ))
                        },
                        |v| Ok(arena.insert(v)),
                    )?;
                    Ok(Value::Pair {
                        fst: elem1,
                        snd: elem2,
                    })
                }

                "Option" => {
                    let elem = seq.next_element::<Option<Value>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| {
                            let x = x.map(|x| arena.insert(x));
                            Ok(Value::Option(x))
                        },
                    )
                }
                _ => Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &"value",
                )),
            },
        )
    }
}
impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_tuple(2, ValueVisitor)
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use Value::*;

        let arena = unsafe { &mut ARENA };
        match self {
            Closure {
                opt_arg: _,
                call: _,
            } => Err(serde::ser::Error::custom("Cant serialize a closure")),
            Int(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Int")?;
                seq.serialize_element(&x.to_string_radix(10))?;
                seq.end()
            }
            Bool(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Bool")?;
                seq.serialize_element(&x)?;
                seq.end()
            }
            Bytes(b) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Bytes")?;
                seq.serialize_element(
                    &std::string::String::from_utf8(b.clone())
                        .map_err(serde::ser::Error::custom)?,
                )?;
                seq.end()
            }
            String(b) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("String")?;
                seq.serialize_element(&b)?;
                seq.end()
            }
            Ticket(b) => {
                let table = unsafe { &mut TICKETS };
                let ticket = { table.remove(b) }.map_or_else(
                    || {
                        Err(serde::ser::Error::custom(
                            "cant serialize non existing ticket",
                        ))
                    },
                    Ok,
                )?;
                unsafe { CONSUMEDTICKETS.push((ticket.clone().ticket_id, *b)) }
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Ticket")?;
                seq.serialize_element(&ticket)?;
                seq.end()
            }
            Union(union) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Union")?;
                seq.serialize_element(&union)?;
                seq.end()
            }
            Map(map) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Map")?;
                let serialized = map.iter().collect::<Vec<(&Value, &Value)>>();
                seq.serialize_element(&serialized)?;
                seq.end()
            }
            Set(set) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Set")?;

                let serialized: Vec<&Value> = set.into_iter().collect();
                seq.serialize_element(&serialized)?;
                seq.end()
            }
            Unit => {
                let mut seq = serializer.serialize_tuple(1)?;
                seq.serialize_element("Unit")?;
                seq.end()
            }
            Option(opt) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Option")?;
                seq.serialize_element(&opt.map(|x| arena.get(x)))?;
                seq.end()
            }
            Pair { fst, snd } => {
                let mut seq = serializer.serialize_tuple(3)?;
                seq.serialize_element("Pair")?;
                let value1 = arena
                    .get(*fst)
                    .map_or_else(|| Err(serde::ser::Error::custom(&"error serializing")), Ok)?;
                let value2 = arena
                    .get(*snd)
                    .map_or_else(|| Err(serde::ser::Error::custom(&"error serializing")), Ok)?;
                seq.serialize_element(value1)?;

                seq.serialize_element(value2)?;
                seq.end()
            }
            List(lst, _) => {
                let mut seq = serializer.serialize_tuple(3)?;
                seq.serialize_element("List")?;
                seq.serialize_element(&lst)?;
                seq.end()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use once_cell::unsync::Lazy;
    use slotmap::HopSlotMap;

    use super::*;
    #[test]
    fn serialization_deserialization_yields_same_structures() {
        let arena = unsafe { &mut ARENA };

        let refre2 = arena.insert(Value::Int(1.into()));
        let refre4 = arena.insert(Value::Int(1.into()));
        let expected = Value::Union(Union::Left(arena.insert(Value::Pair {
            fst: refre2,
            snd: refre4,
        })));

        let ser = &serde_json::to_string(&expected).unwrap();
        unsafe { ARENA = Lazy::new(HopSlotMap::new) };
        let x: Value = serde_json::from_str(ser).unwrap();
        // same keys
        assert_eq!(x, expected);
        let ser2 = &serde_json::to_string(&x).unwrap();
        // same serialized
        assert_eq!(ser, ser2);
    }
}
