use crate::{
    errors::VMResult,
    ticket_table::{Ticket, TicketId, TicketTable},
};
use im_rc::{OrdMap, OrdSet, Vector};
use serde::{de::Visitor, ser::SerializeTuple, Deserialize, Serialize};
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Serialize, Deserialize)]
pub enum Union {
    Left(Box<Value>),
    Right(Box<Value>),
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
#[repr(transparent)]
pub struct FromOcaml(pub Union);
unsafe impl Sync for Union {}
unsafe impl Send for Union {}
impl Serialize for FromOcaml {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use Union::*;

        match &self.0 {
            Right(x) => {
                let mut seq = serializer.serialize_tuple(2)?;

                seq.serialize_element("Right")?;
                seq.serialize_element(&x)?;
                seq.end()
            }
            Left(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Left")?;

                seq.serialize_element(&x)?;
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
        seq.next_element::<&str>()?.map_or_else(
            || {
                Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &"value",
                ))
            },
            |x| match x {
                "Left" => {
                    let elem = seq.next_element::<FromOcamlV>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| {
                            let inserted = Box::from(x.0);
                            Ok(Union::Left(inserted))
                        },
                    )
                }
                "Right" => {
                    let elem = seq.next_element::<FromOcamlV>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| {
                            let inserted = Box::from(x.0);
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
impl<'de> Deserialize<'de> for FromOcaml {
    fn deserialize<D>(deserializer: D) -> Result<FromOcaml, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let v = deserializer.deserialize_tuple(2, UnionVisitor)?;
        Ok(FromOcaml(v))
    }
}
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Serialize, Deserialize)]
pub enum Tag {
    Bytes,
    String,
}
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
#[repr(transparent)]
pub struct FromOcamlV(pub Value);

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Serialize, Deserialize, Clone)]
pub enum Value {
    Bytes(Vec<u8>),
    String(String),
    Int(rug::Integer),
    Union(Union),
    Pair {
        fst: Box<Value>,
        snd: Box<Value>,
    },
    Bool(bool),
    Map(OrdMap<Value, Value>),
    Set(OrdSet<Value>),
    List(Vector<Value>, Option<Tag>),
    Unit,
    Option(Option<Box<Value>>),
    Ticket(Ticket),
    RuntimeTicket(usize),
    Closure {
        opt_arg: Option<Box<Value>>,
        call: i32,
    },
}
impl Value {
    pub fn to_runtime_ticket(self, t: &mut TicketTable) -> Self {
        match self {
            Value::Bytes(_) => self,
            Value::String(_) => self,
            Value::Int(_) => self,
            Value::Union(l) => Value::Union(match l {
                Union::Left(l) => Union::Left(Box::from(l.to_runtime_ticket(t))),
                Union::Right(r) => Union::Right(Box::from(r.to_runtime_ticket(t))),
            }),
            Value::Pair { fst, snd } => Self::Pair {
                fst: Box::from(fst.to_runtime_ticket(t)),
                snd: Box::from(snd.to_runtime_ticket(t)),
            },
            Value::Bool(_) => self,
            Value::Map(x) => {
                let res = x
                    .into_iter()
                    .map(|(k, v)| {
                        let fst = k.to_runtime_ticket(t);
                        let snd = v.to_runtime_ticket(t);
                        (fst, snd)
                    })
                    .collect::<OrdMap<Value, Value>>();
                Self::Map(res)
            }
            Value::Set(x) => Value::Set(x.into_iter().map(|x| x.to_runtime_ticket(t)).collect()),
            Value::List(x, tag) => {
                Value::List(x.into_iter().map(|x| x.to_runtime_ticket(t)).collect(), tag)
            }
            Value::Unit => self,
            Value::Option(x) => Value::Option(x.map(|x| Box::from(x.to_runtime_ticket(t)))),
            Value::Ticket(ticket) => {
                let new = Self::RuntimeTicket(t.counter);
                t.merge(ticket);
                t.incr();
                new
            }
            Value::RuntimeTicket(_) => panic!("lifetime error"),
            Value::Closure {
                opt_arg: _,
                call: _,
            } => panic!("lifetime error"),
        }
    }
    pub fn from_runtime_ticket(
        self,
        t: &mut TicketTable,
        to_return: &mut Vec<(TicketId, usize)>,
    ) -> VMResult<Self> {
        match self {
            Value::Bytes(_) => Ok(self),
            Value::String(_) => Ok(self),
            Value::Int(_) => Ok(self),
            Value::Union(l) => Ok(Value::Union(match l {
                Union::Left(l) => Union::Left(Box::from(l.from_runtime_ticket(t, to_return)?)),
                Union::Right(r) => Union::Right(Box::from(r.from_runtime_ticket(t, to_return)?)),
            })),
            Value::Pair { fst, snd } => Ok(Self::Pair {
                fst: Box::from(fst.from_runtime_ticket(t, to_return)?),
                snd: Box::from(snd.from_runtime_ticket(t, to_return)?),
            }),
            Value::Bool(_) => Ok(self),
            Value::Map(x) => {
                let res = x
                    .into_iter()
                    .map(|(k, v)| {
                        let fst = k.from_runtime_ticket(t, to_return)?;
                        let snd = v.from_runtime_ticket(t, to_return)?;
                        Ok((fst, snd))
                    })
                    .collect::<VMResult<Vec<(Value, Value)>>>();
                let res = res?;
                Ok(Self::Map(OrdMap::from_iter(res.into_iter())))
            }
            Value::Set(x) => {
                let res = x
                    .into_iter()
                    .map(|x| x.from_runtime_ticket(t, to_return))
                    .collect::<VMResult<OrdSet<Value>>>();
                let res = res?;
                Ok(Self::Set(res))
            }
            Value::List(x, tag) => {
                let res = x
                    .into_iter()
                    .map(|x| x.from_runtime_ticket(t, to_return))
                    .collect::<VMResult<Vector<Value>>>();
                let res = res?;
                Ok(Self::List(res, tag))
            }
            Value::Unit => Ok(self),
            Value::Option(x) => {
                let res = match x {
                    None => None,
                    Some(x) => {
                        let res = x.from_runtime_ticket(t, to_return)?;
                        Some(Box::from(res))
                    }
                };
                Ok(Value::Option(res))
            }
            Value::Ticket(_) => panic!("lifetime error"),
            Value::RuntimeTicket(ticket) => {
                let ticket_id = t.extract(&ticket)?;
                to_return.push((ticket_id.ticket_id.clone(), ticket_id.amount));
                Ok(Self::Ticket(ticket_id))
            }
            Value::Closure {
                opt_arg: _,
                call: _,
            } => panic!("lifetime error"),
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
                                serde::de::Unexpected::Str("unexpected sequence, expected bool"),
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
                                serde::de::Unexpected::Str("unexpected sequence, expected int"),
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
                                serde::de::Unexpected::Str("unexpected sequence,expected bytes"),
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
                                serde::de::Unexpected::Str("unexpected sequence,expected string"),
                                &self,
                            ))
                        },
                        |x| Ok(Value::String(x.to_owned())),
                    )
                }
                "Union" => {
                    let elem = seq.next_element::<FromOcaml>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence, expected union"),
                                &"test",
                            ))
                        },
                        |x| Ok(Value::Union(x.0)),
                    )
                }
                "List" => {
                    let elem = seq.next_element::<Vector<FromOcamlV>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |x| {
                            let x = x.into_iter().map(|x| x.0).collect::<Vector<Value>>();
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
                                serde::de::Unexpected::Str("unexpected structure in ticket"),
                                &"Value enum",
                            ))
                        },
                        |x| Ok(Value::Ticket(x)),
                    )
                }
                "Map" => {
                    let elem = seq.next_element::<Vec<(FromOcamlV, FromOcamlV)>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |elem| {
                            Ok(Value::Map(OrdMap::from(
                                elem.into_iter()
                                    .map(|(x, y)| (x.0, y.0))
                                    .collect::<Vec<(Value, Value)>>(),
                            )))
                        },
                    )
                }
                "Set" => {
                    let elem = seq.next_element::<Vec<FromOcamlV>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected structure in list"),
                                &"Value enum",
                            ))
                        },
                        |x| {
                            Ok(Value::Set(OrdSet::from(
                                x.into_iter().map(|x| x.0).collect::<Vec<Value>>(),
                            )))
                        },
                    )
                }
                "Pair" => {
                    let elem1 = seq.next_element::<FromOcamlV>()?;
                    let elem1 = elem1.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str(
                                    "unexpected sequence, expected 1st pair element",
                                ),
                                &self,
                            ))
                        },
                        |v| Ok(Box::from(v.0)),
                    )?;

                    let elem2 = seq.next_element::<FromOcamlV>()?;
                    let elem2 = elem2.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_value(
                                serde::de::Unexpected::Str("unxpected value in second pair elem"),
                                &self,
                            ))
                        },
                        |v| Ok(Box::from(v.0)),
                    )?;
                    Ok(Value::Pair {
                        fst: elem1,
                        snd: elem2,
                    })
                }
                "Option" => {
                    let elem = seq.next_element::<Option<FromOcamlV>>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence, expected option"),
                                &"value",
                            ))
                        },
                        |x| {
                            let x = x.map(|x| Box::from(x.0));
                            Ok(Value::Option(x))
                        },
                    )
                }
                x => Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence, expected valid type"),
                    &x,
                )),
            },
        )
    }
}
impl<'de> Deserialize<'de> for FromOcamlV {
    fn deserialize<D>(deserializer: D) -> Result<FromOcamlV, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let res = deserializer.deserialize_tuple(2, ValueVisitor)?;
        Ok(FromOcamlV(res))
    }
}

impl Serialize for FromOcamlV {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use Value::*;

        match &self.0 {
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
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Ticket")?;
                seq.serialize_element(&b)?;
                seq.end()
            }
            RuntimeTicket(_) => Err(serde::ser::Error::custom("cant serialize runtime ticket")),
            Union(union) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Union")?;
                seq.serialize_element(&FromOcaml(union.clone()))?;
                seq.end()
            }
            Map(map) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Map")?;
                let serialized = map
                    .iter()
                    .map(|(x, y)| (FromOcamlV(x.clone()), FromOcamlV(y.clone())))
                    .collect::<Vec<(FromOcamlV, FromOcamlV)>>();
                seq.serialize_element(&serialized)?;
                seq.end()
            }
            Set(set) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Set")?;

                let serialized: Vec<FromOcamlV> =
                    set.into_iter().map(|x| FromOcamlV(x.clone())).collect();
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
                seq.serialize_element(&opt.clone().map(|x| FromOcamlV(*x)))?;
                seq.end()
            }
            Pair { fst, snd } => {
                let mut seq = serializer.serialize_tuple(3)?;
                seq.serialize_element("Pair")?;

                seq.serialize_element(&FromOcamlV(*fst.clone()))?;

                seq.serialize_element(&FromOcamlV(*snd.clone()))?;
                seq.end()
            }
            List(lst, _) => {
                let mut seq = serializer.serialize_tuple(3)?;
                seq.serialize_element("List")?;
                seq.serialize_element(
                    &lst.iter()
                        .map(|x| FromOcamlV(x.clone()))
                        .collect::<Vec<FromOcamlV>>(),
                )?;
                seq.end()
            }
        }
    }
}

// #[cfg(test)]
// mod test {
//     use once_cell::unsync::Lazy;
//     use slotmap::HopSlotMap;

//     use super::*;
//     #[test]
//     fn serialization_deserialization_yields_same_structures() {
//         let arena = unsafe { &mut ARENA };

//         let refre2 = arena.insert(Value::Int(1.into()));
//         let refre4 = arena.insert(Value::Int(1.into()));
//         let expected = Value::Union(Union::Left(arena.insert(Value::Pair {
//             fst: refre2,
//             snd: refre4,
//         })));

//         let ser = &serde_json::to_string(&expected).unwrap();
//         unsafe { ARENA = Lazy::new(HopSlotMap::new) };
//         let x: Value = serde_json::from_str(ser).unwrap();
//         // same keys
//         assert_eq!(x, expected);
//         let ser2 = &serde_json::to_string(&x).unwrap();
//         // same serialized
//         assert_eq!(ser, ser2);
//     }
// }
