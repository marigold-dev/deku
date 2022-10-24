use std::ops::{Add, BitOr, BitXor, Mul, Neg, Sub};

use im_rc::{OrdSet, Vector};
use rug::Integer;
use slotmap::{DefaultKey, KeyData};
use wasmer::{Exports, Function, ImportObject, Store};

use super::value::*;
use crate::{
    arena::{CONSTANTS, PREDEF},
    conversions,
};
use crate::{
    env::Context,
    errors::{ffi::FFIError, vm::VmError, VMResult},
};

pub fn compare(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let cmp_res = (value1).cmp(&value2) as i8;
    let bumped = env.bump(Value::Int(cmp_res.into()));
    conversions::to_i64(bumped)
}

pub fn equal(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let cmp_res = value1.eq(&value2);
    let bumped = env.bump(Value::Bool(cmp_res));
    conversions::to_i64(bumped)
}
pub fn or(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let res: VMResult<Value> = match (value1, value2) {
        (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x || y)),
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.bitor(y))),

        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Two Bools".to_owned(),
        }
        .into()),
    };
    let res = res?;
    let bumped = env.bump(res);
    conversions::to_i64(bumped)
}
pub fn xor(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let res: VMResult<Value> = match (value1, value2) {
        (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x | y)),
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.bitxor(y))),

        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Two Bools".to_owned(),
        }
        .into()),
    };
    let res = res?;
    let bumped = env.bump(res);
    conversions::to_i64(bumped)
}
pub fn and(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let res: VMResult<Value> = match (value1, value2) {
        (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x && y)),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Two Bools".to_owned(),
        }
        .into()),
    };
    let res = res?;
    let bumped = env.bump(res);
    conversions::to_i64(bumped)
}
pub fn neq(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let one: rug::Integer = rug::Integer::from(1);

    let res: VMResult<Value> = match value {
        Value::Int(n) if n == Integer::ZERO => Ok(false),
        Value::Int(n) if n == one => Ok(true),

        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
    .map(Value::Bool);
    let res = res?;
    let bumped = env.bump(res);
    conversions::to_i64(bumped)
}

pub fn eq(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let one: rug::Integer = rug::Integer::from(1);

    let res: VMResult<Value> = match value {
        Value::Int(n) if n == Integer::ZERO => Ok(false),
        Value::Int(n) if n == one => Ok(true),

        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
    .map(Value::Bool);
    let res = res?;
    let bumped = env.bump(res);
    conversions::to_i64(bumped)
}

pub fn not(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let res: VMResult<Value> = match value {
        Value::Bool(n) => Ok(!n),

        _ => Err(FFIError::ExternError {
            value,
            msg: "type mismatch, expected Bool".to_owned(),
        }
        .into()),
    }
    .map(Value::Bool);
    let res = res?;
    let bumped = env.bump(res);
    conversions::to_i64(bumped)
}
pub fn pair(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let fst = Box::from(value1);
    let snd = Box::from(value2);
    let res = Value::Pair { fst, snd };
    let key = env.bump(res);
    conversions::to_i64(key)
}
pub fn unpair(env: &Context, value: Value) -> VMResult<()> {
    env.update_gas(300)?;
    match value {
        Value::Pair { fst, snd } => {
            let fst = conversions::to_i64(env.bump(*fst))?;
            let snd = conversions::to_i64(env.bump(*snd))?;
            env.push_value(snd)?;
            env.push_value(fst)?;

            Ok(())
        }
        _ => Err(FFIError::ExternError {
            value: (value),
            msg: "type mismatch, expected Pair".to_owned(),
        }
        .into()),
    }
}
pub fn car(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Pair { fst, snd: _ } => conversions::to_i64(env.bump(*fst)),
        _ => Err(FFIError::ExternError {
            value: (value),
            msg: "type mismatch, expected Pair".to_owned(),
        }
        .into()),
    }
}
pub fn cdr(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Pair { fst: _, snd } => conversions::to_i64(env.bump(*snd)),
        _ => Err(FFIError::ExternError {
            value: (value),
            msg: "type mismatch, expected Pair".to_owned(),
        }
        .into()),
    }
}
pub fn z_add(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::Int(x), Value::Int(y)) => {
            let res = Value::Int((x).add(y));
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::Int(_), err) | (err, Value::Int(_)) => Err(FFIError::ExternError {
            value: (err),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}
pub fn z_sub(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::Int(x), Value::Int(y)) => {
            let res = Value::Int((x).sub(y));
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::Int(_), err) | (err, Value::Int(_)) => Err(FFIError::ExternError {
            value: (err),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}
pub fn concat_(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::String(x), Value::String(y)) => {
            let mut new = x.clone();
            new.push_str(&y);
            let res = Value::String(new);
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::Bytes(x), Value::Bytes(y)) => {
            let mut new = x.clone();
            new.extend(y.iter());
            let res = Value::Bytes(new);
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::List(x, tag), rest) => {
            env.push_value(env.bump(rest) as i64)?;
            let new = match tag {
                Some(Tag::Bytes) => {
                    let mut res = Vec::with_capacity(500);
                    x.iter().try_for_each(|x| match x {
                        Value::Bytes(x) => {
                            res.extend_from_slice(x);
                            Ok::<(), VmError>(())
                        }
                        x => Err(FFIError::ExternError {
                            value: x.clone(),
                            msg: "type mismatch, expected Bytes".to_owned(),
                        }
                        .into()),
                    })?;
                    let res = res;
                    Ok::<Value, VmError>(Value::Bytes(res))
                }

                Some(Tag::String) => {
                    let mut res = String::with_capacity(500);
                    x.iter().try_for_each(|x| match x {
                        Value::String(x) => {
                            res.push_str(x);
                            Ok::<(), VmError>(())
                        }
                        x => Err(FFIError::ExternError {
                            value: x.clone(),
                            msg: "type mismatch, expected Bytes".to_owned(),
                        }
                        .into()),
                    })?;
                    let res = res;
                    Ok::<Value, VmError>(Value::String(res))
                }
                None => Err(VmError::RuntimeErr(
                    "type mismatch, expected Tag".to_owned(),
                )),
            }?;
            let key = env.bump(new);
            conversions::to_i64(key)
        }
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}
pub fn ediv(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::Int(x), Value::Int(y)) => {
            if y == Integer::ZERO {
                let res = Value::Option(None);
                let key = env.bump(res);
                conversions::to_i64(key)
            } else {
                let (quot, rem) = (x).div_rem_euc(y);
                let fst = Value::Int(quot);
                let snd = Value::Int(rem);
                let fst = Box::from(fst);
                let snd = Box::from(snd);
                let pair = Value::Pair { fst, snd };
                let pair = Box::from(pair);
                let key = env.bump(Value::Option(Some(pair)));
                conversions::to_i64(key)
            }
        }
        (Value::Int(_), err) | (err, Value::Int(_)) => Err(FFIError::ExternError {
            value: (err),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}
pub fn z_mul(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::Int(x), Value::Int(y)) => {
            let res = Value::Int((x).mul(y));
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::Int(_), err) | (err, Value::Int(_)) => Err(FFIError::ExternError {
            value: (err),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}
pub fn lsl(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::Int(x), Value::Int(y)) => {
            let res = Value::Int((x) << (y.to_i32_wrapping()));
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::Int(_), err) | (err, Value::Int(_)) => Err(FFIError::ExternError {
            value: (err),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}
pub fn lsr(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (value1, value2) {
        (Value::Int(x), Value::Int(y)) => {
            let res = Value::Int((x) >> (y.to_i32_wrapping()));
            let key = env.bump(res);
            conversions::to_i64(key)
        }
        (Value::Int(_), err) | (err, Value::Int(_)) => Err(FFIError::ExternError {
            value: (err),
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
        (x, _) => Err(FFIError::ExternError {
            value: x,
            msg: "type mismatch, expected Int".to_owned(),
        }
        .into()),
    }
}

pub fn is_left(env: &Context, value: Value) -> VMResult<i32> {
    env.update_gas(300)?;
    match value {
        Value::Union(Union::Left(l)) => {
            let key = conversions::to_i64(env.bump(*l))?;
            env.push_value(key)?;
            Ok(1)
        }
        Value::Union(Union::Right(l)) => {
            let key = conversions::to_i64(env.bump(*l))?;
            env.push_value(key)?;
            Ok(0)
        }
        _ => Err(FFIError::ExternError {
            value: (value),
            msg: "type mismatch, expected Union".to_owned(),
        }
        .into()),
    }
}
pub fn deref_bool(env: &Context, value: Value) -> VMResult<i32> {
    env.update_gas(300)?;
    match value {
        Value::Bool(x) => Ok((x).into()),
        _ => Err(FFIError::ExternError {
            value: (value),
            msg: "type mismatch, expected Bool".to_owned(),
        }
        .into()),
    }
}
pub fn failwith(env: &Context, value: Value) -> VMResult<()> {
    env.update_gas(300)?;
    match value {
        Value::String(str) => Err(VmError::RuntimeErr(str)),
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected String".to_owned(),
        }
        .into()),
    }
}
pub fn if_none(env: &Context, value: Value) -> VMResult<i32> {
    env.update_gas(300)?;
    match value {
        Value::Option(x) => (x).map_or_else(
            || Ok(1),
            |v| {
                let key = conversions::to_i64(env.bump(*v))?;
                env.push_value(key)?;
                Ok(0)
            },
        ),
        _ => Err(FFIError::ExternError {
            value: (value),
            msg: "type mismatch, expected Option".to_owned(),
        }
        .into()),
    }
}
pub fn if_cons(env: &Context, value: Value) -> VMResult<i32> {
    env.update_gas(300)?;
    match value {
        Value::List(x, tag) if x.len() == 2 => {
            x.last().map_or_else(
                || Err(VmError::RuntimeErr("cant happen".to_owned())),
                |v| {
                    let bumped = env.bump(v.clone());
                    let key = conversions::to_i64(bumped)?;
                    env.push_value(key)?;
                    Ok(())
                },
            )?;
            let rest = x.take(x.len() - 2);

            let bumped = env.bump(Value::List(rest, tag));
            let key = conversions::to_i64(bumped)?;
            env.push_value(key)?;

            Ok(1)
        }
        Value::List(x, _tag) if x.len() == 1 => {
            x.head().map_or_else(
                || Err(VmError::RuntimeErr("cant happen".to_owned())),
                |v| {
                    let bumped = env.bump(v.clone());
                    let key = conversions::to_i64(bumped)?;
                    env.push_value(key)?;
                    Ok(())
                },
            )?;

            Ok(1)
        }
        _ => Ok(0),
    }
}
pub fn is_nat(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match &value {
        Value::Int(x) if x >= &Integer::ZERO => {
            let opt = Value::Option(Some(Box::from(value)));
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        Value::Int(_) => {
            let opt = Value::Option(None);
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
pub fn abs(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Int(x) => {
            let opt = Value::Int(x.abs());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
pub fn neg(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Int(x) => {
            let opt = Value::Int(x.neg());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
use blake2::digest::consts::U32;
use blake2::Digest;
pub type Blake2b256 = blake2::Blake2b<U32>;

fn blake2b(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Bytes(x) => {
            let opt = Value::Bytes(Blake2b256::digest(x).to_vec());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
fn unpack(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match &value {
        Value::Bytes(x) => {
            let opt: VMResult<Value> = bincode::deserialize(x).map_err(|_| {
                FFIError::ExternError {
                    value: (value).clone(),
                    msg: "failed to unpack".to_owned(),
                }
                .into()
            });
            let opt = opt?;
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "unpack type mismatch, expected Bytes".to_owned(),
        }
        .into()),
    }
}
fn pack(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;

    let opt: VMResult<Vec<u8>> = bincode::serialize(&value).map_err(|_| {
        FFIError::ExternError {
            value: (value).clone(),
            msg: "failed to unpack".to_owned(),
        }
        .into()
    });
    let opt = opt?;
    let bumped = env.bump(Value::Bytes(opt));
    let key = conversions::to_i64(bumped)?;
    Ok(key)
}
fn sha3(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Bytes(x) => {
            let opt = Value::Bytes(sha3::Sha3_256::digest(x).to_vec());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
fn sha256(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Bytes(x) => {
            let opt = Value::Bytes(<sha2::Sha256 as sha2::Digest>::digest(&x).to_vec());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
fn sha512(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Bytes(x) => {
            let opt = Value::Bytes(<sha2::Sha512 as sha2::Digest>::digest(&x).to_vec());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
fn keccak(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Bytes(x) => {
            let opt = Value::Bytes(sha3::Keccak256::digest(x).to_vec());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
pub fn size(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value {
        Value::Map(x) => {
            let opt = Value::Int(x.len().into());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        Value::List(x, _) => {
            let opt = Value::Int(x.len().into());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        Value::Set(x) => {
            let opt = Value::Int(x.len().into());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        Value::Bytes(x) => {
            let opt = Value::Int(x.len().into());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        Value::String(x) => {
            let opt = Value::Int(x.len().into());
            let bumped = env.bump(opt);
            let key = conversions::to_i64(bumped)?;
            Ok(key)
        }
        _ => Err(FFIError::ExternError {
            value: (value).clone(),
            msg: "type mismatch, expected Nat".to_owned(),
        }
        .into()),
    }
}
pub fn lt(env: &Context, value1: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let cmp_res = (value1).lt(&Value::Int(Integer::ZERO)) as bool;
    let bumped = env.bump(Value::Bool(cmp_res));
    conversions::to_i64(bumped)
}
pub fn gt(env: &Context, value1: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let cmp_res = (value1).gt(&Value::Int(Integer::ZERO));
    let bumped = env.bump(Value::Bool(cmp_res));
    conversions::to_i64(bumped)
}
pub fn le(env: &Context, value1: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let cmp_res = (value1).le(&Value::Int(Integer::ZERO)) as bool;
    let bumped = env.bump(Value::Bool(cmp_res));
    conversions::to_i64(bumped)
}
pub fn ge(env: &Context, value1: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let cmp_res = (value1).ge(&Value::Int(Integer::ZERO));
    let bumped = env.bump(Value::Bool(cmp_res));
    conversions::to_i64(bumped)
}
pub fn closure(env: &Context, value1: i32) -> VMResult<i64> {
    env.update_gas(300)?;
    let clos = Value::Closure {
        opt_arg: None,
        call: value1,
    };
    let bumped = env.bump(clos);
    conversions::to_i64(bumped)
}
pub fn some(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let opt = Value::Option(Some(Box::from(value)));
    let bumped = env.bump(opt);
    let key = conversions::to_i64(bumped)?;
    Ok(key)
}
pub fn left(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let opt = Value::Union(Union::Left(Box::from(value)));
    let bumped = env.bump(opt);
    let key = conversions::to_i64(bumped)?;
    Ok(key)
}
pub fn right(env: &Context, value: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    let bumped = Box::from(value);
    let opt = Value::Union(Union::Right(bumped));
    let bumped = env.bump(opt);
    let key = conversions::to_i64(bumped)?;
    Ok(key)
}
pub fn get_n(env: &Context, idx: u32, value: Value) -> VMResult<i64> {
    env.update_gas(300 * u64::from(idx))?;
    if idx == 0 {
        let bumped = env.bump(value);
        let key = conversions::to_i64(bumped)?;
        return Ok(key);
    }
    let mut current = value;
    let mut loop_idx = idx;
    loop {
        if loop_idx == 0 {
            let bumped = env.bump(current);
            let key = conversions::to_i64(bumped)?;
            return Ok(key);
        }
        match (loop_idx, current) {
            (1, Value::Pair { fst, snd: _ }) => {
                current = *fst;
                break;
            }
            (2, Value::Pair { fst: _, snd }) => {
                current = *snd;
                break;
            }
            (_, Value::Pair { fst: _, snd }) => {
                current = *snd;
                loop_idx = loop_idx.saturating_sub(2);
            }
            (_, value) => {
                return Err(FFIError::ExternError {
                    value: (value),
                    msg: "type mismatch, expected Pair, get_n".to_owned(),
                }
                .into())
            }
        }
    }
    let bumped = env.bump(current);
    let key = conversions::to_i64(bumped)?;
    Ok(key)
}

pub fn mem(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value1 {
        Value::Map(x) => {
            let res = x.contains_key(&value2);
            let bumped = env.bump(Value::Bool(res));
            conversions::to_i64(bumped)
        }
        Value::Set(x) => {
            let res = x.contains(&value2);
            let bumped = env.bump(Value::Bool(res));
            conversions::to_i64(bumped)
        }
        _ => Err(FFIError::ExternError {
            value: value1,
            msg: "type mismatch, expected Map/Set with a Key".to_owned(),
        }
        .into()),
    }
}
pub fn map_get(env: &Context, value1: Value, value2: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match value2 {
        Value::Map(x) => {
            let res = x.get(&value1);
            let bumped = res.map(|res| Box::from(res.clone()));
            let bumped = env.bump(Value::Option(bumped));
            conversions::to_i64(bumped)
        }
        _ => Err(FFIError::ExternError {
            value: value2,
            msg: "type mismatch, expected Map with a Key".to_owned(),
        }
        .into()),
    }
}
pub fn update(env: &Context, key: Value, value: Value, map: Value) -> VMResult<i64> {
    env.update_gas(300)?;
    match (&map, value) {
        (Value::Map(x), Value::Option(boxed)) => {
            let mut map = x.clone();
            match boxed {
                None => {
                    map.remove(&key);
                }
                Some(x) => {
                    let x = *x;
                    map.insert(key, x);
                }
            }
            let bumped = env.bump(Value::Map(map));
            conversions::to_i64(bumped)
        }
        (Value::Set(x), Value::Bool(cond)) => {
            let mut x = x.clone();
            match cond {
                false => {
                    x.remove(&key);
                }
                true => {
                    x.insert(key);
                }
            }
            let bumped = env.bump(Value::Set(x));
            conversions::to_i64(bumped)
        }
        _ => Err(FFIError::ExternError {
            value: map.clone(),
            msg: "type mismatch, expected Map with a Option Value".to_owned(),
        }
        .into()),
    }
}
pub fn get_and_update(env: &Context, key: Value, value: Value, map: Value) -> VMResult<()> {
    env.update_gas(300)?;
    match (&map, value) {
        (Value::Map(x), Value::Option(boxed)) => {
            let mut map = x.clone();
            let mut retur = None;
            match boxed {
                None => {
                    retur = map.remove(&key);
                }
                Some(x) => {
                    let x = *x;
                    map.insert(key, x);
                }
            }
            let bumped = env.bump(Value::Map(map));
            let bumped2 = retur.map(Box::from);
            let bumped2 = env.bump(Value::Option(bumped2));
            env.push_value(bumped as i64)?;
            env.push_value(bumped2 as i64)?;
            Ok(())
        }
        _ => Err(FFIError::ExternError {
            value: map.clone(),
            msg: "type mismatch, expected Map with a Option Value".to_owned(),
        }
        .into()),
    }
}
pub const fn call1<A, F>(f: F) -> impl Fn(&Context, i64) -> VMResult<A>
where
    F: Fn(&Context, Value) -> VMResult<A>,
{
    move |env, arg| match env.get(DefaultKey::from(KeyData::from_ffi(arg as u64))) {
        Ok(x) => f(env, x),
        Err(x) => Err(x),
    }
}
pub const fn call2<F, A>(f: F) -> impl Fn(&Context, i64, i64) -> VMResult<A>
where
    F: Fn(&Context, Value, Value) -> VMResult<A>,
{
    move |env, arg, arg2| match (
        env.get(DefaultKey::from(KeyData::from_ffi(arg as u64))),
        env.get(DefaultKey::from(KeyData::from_ffi(arg2 as u64))),
    ) {
        (Ok(x), Ok(y)) => f(env, x, y),
        (_, _) => Err(VmError::RuntimeErr("illegal argument".to_owned())),
    }
}

pub const fn call2_extra<F, A>(f: F) -> impl Fn(&Context, u32, i64) -> VMResult<A>
where
    F: Fn(&Context, u32, Value) -> VMResult<A>,
{
    move |env, arg, arg2| match env.get(DefaultKey::from(KeyData::from_ffi(arg2 as u64))) {
        Ok(x) => f(env, arg, x),
        _ => Err(VmError::RuntimeErr("illegal argument".to_owned())),
    }
}
pub const fn call2_mapping<F, A>(f: F) -> impl Fn(&Context, i64, i32) -> VMResult<A>
where
    F: Fn(&Context, Value, i32) -> VMResult<A>,
{
    move |env, arg, arg2| match env.get(DefaultKey::from(KeyData::from_ffi(arg as u64))) {
        Ok(x) => f(env, x, arg2),
        _ => Err(VmError::RuntimeErr("illegal argument".to_owned())),
    }
}
pub const fn call2_default<F, A>(f: F) -> impl Fn(&Context, u32, i64) -> VMResult<A>
where
    F: Fn(&Context, DefaultKey, DefaultKey) -> VMResult<A>,
{
    move |env, arg, arg2| {
        let (x, y) = (
            DefaultKey::from(KeyData::from_ffi(arg as u64)),
            DefaultKey::from(KeyData::from_ffi(arg2 as u64)),
        );
        f(env, x, y)
    }
}
pub const fn call2_default_value<F, A>(f: F) -> impl Fn(&Context, u32, i64) -> VMResult<A>
where
    F: Fn(&Context, Value, DefaultKey) -> VMResult<A>,
{
    move |env, arg, arg2| {
        let (x, y) = (
            env.get(DefaultKey::from(KeyData::from_ffi(arg as u64)))?,
            DefaultKey::from(KeyData::from_ffi(arg2 as u64)),
        );
        f(env, x, y)
    }
}
pub const fn call3<F, A>(f: F) -> impl Fn(&Context, i64, i64, i64) -> VMResult<A>
where
    F: Fn(&Context, Value, Value, Value) -> VMResult<A>,
{
    move |env, arg, arg2, arg3| match (
        env.get(DefaultKey::from(KeyData::from_ffi(arg as u64))),
        env.get(DefaultKey::from(KeyData::from_ffi(arg3 as u64))),
        env.get(DefaultKey::from(KeyData::from_ffi(arg2 as u64))),
    ) {
        (Ok(x), Ok(y), Ok(z)) => f(env, x, z, y),
        _ => Err(VmError::RuntimeErr("illegal argument".to_owned())),
    }
}

pub fn make_imports(env: &Context, store: &Store) -> ImportObject {
    let mut imports = ImportObject::new();
    let mut exports = Exports::new();

    exports.insert(
        "compare",
        Function::new_native_with_env(store, env.clone(), call2(compare)),
    );
    exports.insert(
        "equal",
        Function::new_native_with_env(store, env.clone(), call2(equal)),
    );
    exports.insert(
        "or",
        Function::new_native_with_env(store, env.clone(), call2(or)),
    );
    exports.insert(
        "and",
        Function::new_native_with_env(store, env.clone(), call2(and)),
    );
    exports.insert(
        "lt",
        Function::new_native_with_env(store, env.clone(), call1(lt)),
    );
    exports.insert(
        "blake2b",
        Function::new_native_with_env(store, env.clone(), call1(blake2b)),
    );
    exports.insert(
        "pack",
        Function::new_native_with_env(store, env.clone(), call1(pack)),
    );
    exports.insert(
        "unpack",
        Function::new_native_with_env(store, env.clone(), call1(unpack)),
    );
    exports.insert(
        "sha3",
        Function::new_native_with_env(store, env.clone(), call1(sha3)),
    );
    exports.insert(
        "sha256",
        Function::new_native_with_env(store, env.clone(), call1(sha256)),
    );
    exports.insert(
        "sha512",
        Function::new_native_with_env(store, env.clone(), call1(sha512)),
    );
    exports.insert(
        "keccak",
        Function::new_native_with_env(store, env.clone(), call1(keccak)),
    );
    exports.insert(
        "lt",
        Function::new_native_with_env(store, env.clone(), call1(lt)),
    );
    exports.insert(
        "gt",
        Function::new_native_with_env(store, env.clone(), call1(gt)),
    );
    exports.insert(
        "le",
        Function::new_native_with_env(store, env.clone(), call1(le)),
    );
    exports.insert(
        "ge",
        Function::new_native_with_env(store, env.clone(), call1(ge)),
    );
    exports.insert(
        "closure",
        Function::new_native_with_env(store, env.clone(), closure),
    );
    exports.insert(
        "neq",
        Function::new_native_with_env(store, env.clone(), call1(neq)),
    );
    exports.insert(
        "neg",
        Function::new_native_with_env(store, env.clone(), call1(neg)),
    );
    exports.insert(
        "eq",
        Function::new_native_with_env(store, env.clone(), call1(eq)),
    );
    exports.insert(
        "not",
        Function::new_native_with_env(store, env.clone(), call1(not)),
    );
    exports.insert(
        "pair",
        Function::new_native_with_env(store, env.clone(), call2(pair)),
    );
    exports.insert(
        "unpair",
        Function::new_native_with_env(store, env.clone(), call1(unpair)),
    );
    exports.insert(
        "car",
        Function::new_native_with_env(store, env.clone(), call1(car)),
    );
    exports.insert(
        "cdr",
        Function::new_native_with_env(store, env.clone(), call1(cdr)),
    );
    exports.insert(
        "size",
        Function::new_native_with_env(store, env.clone(), call1(size)),
    );
    exports.insert(
        "z_add",
        Function::new_native_with_env(store, env.clone(), call2(z_add)),
    );
    exports.insert(
        "z_mul",
        Function::new_native_with_env(store, env.clone(), call2(z_mul)),
    );
    exports.insert(
        "lsl",
        Function::new_native_with_env(store, env.clone(), call2(lsl)),
    );
    exports.insert(
        "lsr",
        Function::new_native_with_env(store, env.clone(), call2(lsr)),
    );
    exports.insert(
        "xor",
        Function::new_native_with_env(store, env.clone(), call2(xor)),
    );
    exports.insert(
        "z_sub",
        Function::new_native_with_env(store, env.clone(), call2(z_sub)),
    );
    exports.insert(
        "concat",
        Function::new_native_with_env(store, env.clone(), call2(concat_)),
    );
    exports.insert(
        "ediv",
        Function::new_native_with_env(store, env.clone(), call2(ediv)),
    );
    exports.insert(
        "if_left",
        Function::new_native_with_env(store, env.clone(), call1(is_left)),
    );
    exports.insert(
        "deref_bool",
        Function::new_native_with_env(store, env.clone(), call1(deref_bool)),
    );
    exports.insert(
        "failwith",
        Function::new_native_with_env(store, env.clone(), call1(failwith)),
    );
    exports.insert(
        "if_none",
        Function::new_native_with_env(store, env.clone(), call1(if_none)),
    );
    exports.insert(
        "if_cons",
        Function::new_native_with_env(store, env.clone(), call1(if_cons)),
    );
    exports.insert(
        "isnat",
        Function::new_native_with_env(store, env.clone(), call1(is_nat)),
    );
    exports.insert(
        "abs",
        Function::new_native_with_env(store, env.clone(), call1(abs)),
    );
    exports.insert(
        "int",
        Function::new_native_with_env(store, env.clone(), int),
    );
    exports.insert(
        "implicit_account",
        Function::new_native_with_env(store, env.clone(), address),
    );
    exports.insert(
        "some",
        Function::new_native_with_env(store, env.clone(), call1(some)),
    );
    exports.insert(
        "left",
        Function::new_native_with_env(store, env.clone(), call1(left)),
    );
    exports.insert(
        "right",
        Function::new_native_with_env(store, env.clone(), call1(right)),
    );
    exports.insert(
        "get_n",
        Function::new_native_with_env(store, env.clone(), call2_extra(get_n)),
    );
    exports.insert(
        "mem",
        Function::new_native_with_env(store, env.clone(), call2(mem)),
    );
    exports.insert(
        "map_get",
        Function::new_native_with_env(store, env.clone(), call2(map_get)),
    );
    exports.insert(
        "update",
        Function::new_native_with_env(store, env.clone(), call3(update)),
    );
    exports.insert(
        "get_and_update",
        Function::new_native_with_env(store, env.clone(), call3(get_and_update)),
    );
    exports.insert(
        "transfer_tokens",
        Function::new_native_with_env(store, env.clone(), call3(transfer_tokens)),
    );
    exports.insert(
        "nil",
        Function::new_native_with_env(store, env.clone(), nil),
    );
    exports.insert(
        "amount",
        Function::new_native_with_env(store, env.clone(), zero),
    );
    exports.insert(
        "empty_set",
        Function::new_native_with_env(store, env.clone(), empty_set),
    );
    exports.insert(
        "empty_map",
        Function::new_native_with_env(store, env.clone(), empty_map),
    );
    exports.insert(
        "empty_big_map",
        Function::new_native_with_env(store, env.clone(), empty_map),
    );
    exports.insert(
        "cons",
        Function::new_native_with_env(store, env.clone(), call2(cons)),
    );
    exports.insert(
        "const",
        Function::new_native_with_env(store, env.clone(), const_),
    );
    exports.insert(
        "none",
        Function::new_native_with_env(store, env.clone(), none),
    );
    exports.insert(
        "zero",
        Function::new_native_with_env(store, env.clone(), zero),
    );
    exports.insert(
        "unit",
        Function::new_native_with_env(store, env.clone(), unit),
    );
    exports.insert(
        "sender",
        Function::new_native_with_env(store, env.clone(), sender),
    );
    exports.insert(
        "source",
        Function::new_native_with_env(store, env.clone(), source),
    );
    exports.insert(
        "self",
        Function::new_native_with_env(store, env.clone(), self_),
    );
    exports.insert(
        "true",
        Function::new_native_with_env(store, env.clone(), true_),
    );
    exports.insert(
        "false",
        Function::new_native_with_env(store, env.clone(), false_),
    );
    exports.insert(
        "balance",
        Function::new_native_with_env(store, env.clone(), zero),
    );
    exports.insert(
        "amount",
        Function::new_native_with_env(store, env.clone(), zero),
    );
    exports.insert(
        "self_address",
        Function::new_native_with_env(store, env.clone(), self_),
    );
    exports.insert(
        "iter",
        Function::new_native_with_env(store, env.clone(), call2_mapping(iter)),
    );
    exports.insert(
        "address",
        Function::new_native_with_env(store, env.clone(), address),
    );
    exports.insert(
        "contract",
        Function::new_native_with_env(store, env.clone(), contract),
    );
    exports.insert(
        "dup_host",
        Function::new_native_with_env(store, env.clone(), dup_host),
    );
    exports.insert(
        "map",
        Function::new_native_with_env(store, env.clone(), call2_mapping(map)),
    );
    exports.insert(
        "exec",
        Function::new_native_with_env(store, env.clone(), call2(exec)),
    );
    exports.insert(
        "apply",
        Function::new_native_with_env(store, env.clone(), call2(apply)),
    );
    // TODO!
    exports.insert(
        "ticket",
        Function::new_native_with_env(store, env.clone(), call2(ticket)),
    );
    exports.insert(
        "read_ticket",
        Function::new_native_with_env(store, env.clone(), call1(read_ticket)),
    );
    exports.insert(
        "split_ticket",
        Function::new_native_with_env(store, env.clone(), call2(split_ticket)),
    );
    exports.insert(
        "join_tickets",
        Function::new_native_with_env(store, env.clone(), call1(join_tickets)),
    );
    imports.register("env", exports);
    imports
}
fn ticket(env: &Context, payload: Value, amount: Value) -> VMResult<i64> {
    match (payload, amount) {
        (Value::Bytes(x), Value::Int(y)) => {
            let predef = unsafe { &PREDEF };
            if let Value::String(nil) = predef
                .get("self")
                .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?
            {
                let handle = env.with_table(|table| {
                    let string = String::from_utf8_lossy(&x);
                    let handle =
                        table.mint_ticket(nil.clone(), y.to_usize_wrapping(), string.to_string());
                    Ok(Value::RuntimeTicket(handle))
                })?;
                Ok(env.bump(handle) as i64)
            } else {
                Err(VmError::RuntimeErr(
                    "cant mint ticket, wrong values supplied".to_owned(),
                ))
            }
        }
        _ => Err(VmError::RuntimeErr(
            "cant mint ticket, wrong values supplied".to_owned(),
        )),
    }
}
fn join_tickets(env: &Context, payload: Value) -> VMResult<i64> {
    if let Value::Pair { fst, snd } = payload {
        match (*fst, *snd) {
            (Value::RuntimeTicket(x), Value::RuntimeTicket(y)) => {
                let handle = env.with_table(|table| {
                    table
                        .join_tickets((&x, &y))
                        .map_err(std::convert::Into::into)
                });
                handle.map_or_else(
                    |_| Ok(env.bump(Value::Option(None)) as i64),
                    |ok| {
                        let ticket = Value::RuntimeTicket(ok);
                        let value = Value::Option(Some(Box::from(ticket)));
                        Ok(env.bump(value) as i64)
                    },
                )
            }
            _ => Err(VmError::RuntimeErr(
                "cant join ticket, wrong values supplied".to_owned(),
            )),
        }
    } else {
        Err(VmError::RuntimeErr(
            "cant join ticket, wrong values supplied".to_owned(),
        ))
    }
}
fn split_ticket(env: &Context, payload: Value, nat: Value) -> VMResult<i64> {
    if let (Value::Pair { fst, snd }, Value::RuntimeTicket(x)) = (nat, payload) {
        match (*fst, *snd) {
            (Value::Int(x1), Value::Int(x2)) => {
                let handle = env.with_table(|table| {
                    table
                        .split_ticket(&x, (x1.to_usize_wrapping(), x2.to_usize_wrapping()))
                        .map_err(std::convert::Into::into)
                });
                handle.map_or_else(
                    |_| Ok(env.bump(Value::Option(None)) as i64),
                    |(h1, h2)| {
                        let ticket1 = Box::from(Value::RuntimeTicket(h1));
                        let ticket2 = Box::from(Value::RuntimeTicket(h2));

                        let value = Value::Pair {
                            fst: ticket1,
                            snd: ticket2,
                        };
                        Ok(env.bump(value) as i64)
                    },
                )
            }
            _ => Err(VmError::RuntimeErr(
                "cant join ticket, wrong values supplied".to_owned(),
            )),
        }
    } else {
        Err(VmError::RuntimeErr(
            "cant join ticket, wrong values supplied".to_owned(),
        ))
    }
}
fn read_ticket(env: &Context, payload: Value) -> VMResult<()> {
    match payload {
        Value::RuntimeTicket(x) => {
            let (ticket_id, amount, handle) =
                env.with_table(|table| Ok(table.read_ticket(&x)))??;
            let address = ticket_id.ticketer;
            let value = ticket_id.data;
            let amount = amount;
            let address = Box::from(Value::String(address));
            let value = Box::from(Value::Bytes(value.as_bytes().to_vec()));
            let amount = Box::from(Value::Int(amount.into()));
            let p1 = Value::Pair {
                fst: value,
                snd: amount,
            };
            let p1 = Box::from(p1);
            let p1 = Value::Pair {
                fst: address,
                snd: p1,
            };
            let p1 = env.bump(p1);

            env.push_value(env.bump(Value::RuntimeTicket(handle)) as i64)?;
            env.push_value(p1 as i64)?;

            Ok(())
        }
        _ => Err(VmError::RuntimeErr(
            "cant mint ticket, wrong values supplied".to_owned(),
        )),
    }
}
fn nil(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("nil")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn true_(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("true")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn false_(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("false")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn unit(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("unit")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn const_(c: &Context, idx: i32) -> VMResult<i64> {
    let predef = unsafe { &CONSTANTS };
    let nil = predef
        .get(idx as usize)
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn empty_map(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("empty_map")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn empty_set(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("empty_set")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}

fn zero(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("zero")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn self_(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("self")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn sender(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("sender")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn source(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("source")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn cons(c: &Context, v1: Value, v2: Value) -> VMResult<i64> {
    match v2 {
        Value::List(x, tag) => {
            let mut x = x;
            x.push_front(v1);
            let lst = Value::List(x, tag);
            let bumped = c.bump(lst);
            conversions::to_i64(bumped)
        }
        _ => Err(VmError::RuntimeErr("illegal argument".to_owned())),
    }
}
fn transfer_tokens(env: &Context, v1: Value, v2: Value, v3: Value) -> VMResult<i64> {
    if let Value::Int(x) = v2 {
        if x != Integer::ZERO {
            return Err(VmError::RuntimeErr("illegal argument".to_owned()));
        }
        let fst = Box::from(v1);
        let snd = Box::from(v3);
        let pair = Value::Pair { fst, snd };
        let bumped = env.bump(pair);
        conversions::to_i64(bumped)
    } else {
        Err(VmError::RuntimeErr("illegal argument".to_owned()))
    }
}
fn none(c: &Context) -> VMResult<i64> {
    let predef = unsafe { &PREDEF };
    let nil = predef
        .get("none")
        .map_or_else(|| Err(VmError::RuntimeErr("cant happen".to_owned())), Ok)?;
    let bumped = c.bump(nil.clone());
    conversions::to_i64(bumped)
}
fn map(env: &Context, v: Value, idx: i32) -> VMResult<i64> {
    match v {
        Value::List(x, _) => {
            let new: VMResult<Vector<Value>> = x
                .iter()
                .map(|x| {
                    let reff = x.clone();
                    let bumped = env.bump(reff);
                    let res = env.call(bumped as i64, idx)?;
                    env.get(DefaultKey::from(KeyData::from_ffi(res as u64)))
                })
                .collect();
            let val = new?;
            let tag = match val.head() {
                Some(Value::Bytes(_)) => Some(Tag::Bytes),
                Some(Value::String(_)) => Some(Tag::String),
                _ => None,
            };
            let bumped = env.bump(Value::List(val, tag));
            Ok(bumped as i64)
        }
        Value::Set(x) => {
            let new: VMResult<OrdSet<Value>> = x
                .iter()
                .map(|x| {
                    let reff = x.clone();
                    let bumped = env.bump(reff);
                    let res = env.call(bumped as i64, idx)?;
                    env.get(DefaultKey::from(KeyData::from_ffi(res as u64)))
                })
                .collect();
            let val = new?;
            let bumped = env.bump(Value::Set(val));
            Ok(bumped as i64)
        }
        Value::Map(x) => {
            let new: VMResult<OrdSet<Value>> = x
                .into_iter()
                .map(|(k, v)| {
                    let k = k;
                    let v = v;

                    let k = Box::from(k);
                    let v = Box::from(v);
                    let pair = Value::Pair { fst: k, snd: v };
                    let bumped = env.bump(pair);
                    let res = env.call(bumped as i64, idx)?;
                    env.get(DefaultKey::from(KeyData::from_ffi(res as u64)))
                })
                .collect();
            let val = new?;
            let bumped = env.bump(Value::Set(val));
            Ok(bumped as i64)
        }
        _ => Err(FFIError::ExternError {
            value: v.clone(),
            msg: "type mismatch, expected Map with a Option Value".to_owned(),
        }
        .into()),
    }
}
fn exec(env: &Context, v: Value, lam: Value) -> VMResult<i64> {
    let v = &v;
    if let Value::Closure { opt_arg, call } = lam {
        opt_arg.map_or_else(
            || {
                let bumped = Box::from(v.clone());

                env.call(env.bump(*bumped) as i64, call)
            },
            |x| {
                let bumped = Box::from(v.clone());

                let p = Value::Pair {
                    fst: bumped,
                    snd: x,
                };
                let p = env.bump(p);
                env.call(p as i64, call)
            },
        )
    } else {
        Err(FFIError::ExternError {
            value: lam,
            msg: "type mismatch, expected Lambda".to_owned(),
        }
        .into())
    }
}

fn apply(env: &Context, v: Value, lam: Value) -> VMResult<i64> {
    let v = &v;
    if let Value::Closure { opt_arg, call } = lam {
        opt_arg.map_or_else(
            || {
                let bumped = Box::from(v.clone());

                let p = Value::Closure {
                    opt_arg: Some(bumped),
                    call,
                };
                let p = env.bump(p);
                Ok(p as i64)
            },
            |x| {
                let bumped = Box::from(v.clone());

                let res = Value::Pair {
                    fst: bumped,
                    snd: x,
                };
                let p = Value::Closure {
                    opt_arg: Some(Box::from(res)),
                    call,
                };
                let p = env.bump(p);
                Ok(p as i64)
            },
        )
    } else {
        Err(FFIError::ExternError {
            value: lam,
            msg: "type mismatch, expected Lambda".to_owned(),
        }
        .into())
    }
}
fn iter(env: &Context, v: Value, idx: i32) -> VMResult<()> {
    match v {
        Value::List(x, _) => x.iter().try_for_each(|x| {
            let reff = x.clone();
            let bumped = env.bump(reff);
            env.call_unit(bumped as i64, idx)
        }),
        Value::Set(x) => x.iter().try_for_each(|x| {
            let reff = x.clone();
            let bumped = env.bump(reff);
            env.call_unit(bumped as i64, idx)
        }),
        Value::Map(x) => x.into_iter().try_for_each(|(k, v)| {
            let k = k;
            let v = v;

            let pair = Value::Pair {
                fst: Box::from(k),
                snd: Box::from(v),
            };
            let bumped = env.bump(pair);
            env.call_unit(bumped as i64, idx)
        }),
        _ => Err(FFIError::ExternError {
            value: v.clone(),
            msg: "type mismatch, expected Map with a Option Value".to_owned(),
        }
        .into()),
    }
}

fn dup_host(c: &Context, v: i64) -> VMResult<()> {
    let v = DefaultKey::from(KeyData::from_ffi(v as u64));
    let v = c.get_ref(v)?;
    let cloned = v.clone();
    let bumped = c.bump(cloned);
    let conved = conversions::to_i64(bumped)?;
    c.push_value(conved)
}
fn address(c: &Context, v: i64) -> VMResult<i64> {
    let v = DefaultKey::from(KeyData::from_ffi(v as u64));
    let v = c.get(v)?;

    let bumped = c.bump(v);
    let conved = conversions::to_i64(bumped)?;
    Ok(conved)
}
fn int(_c: &Context, v: i64) -> VMResult<i64> {
    Ok(v)
}

fn contract(c: &Context, v: i64) -> VMResult<i64> {
    let v = DefaultKey::from(KeyData::from_ffi(v as u64));
    let v = c.get(v)?;

    let opt = Value::Option(Some(Box::from(v)));
    let bumped = c.bump(opt);

    let conved = conversions::to_i64(bumped)?;
    Ok(conved)
}
