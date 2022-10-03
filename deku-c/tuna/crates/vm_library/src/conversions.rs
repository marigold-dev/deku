use crate::errors::VMResult;

pub fn to_i64(input: u64) -> VMResult<i64> {
    Ok(input as i64)
}

#[cfg(test)]
mod test {
    #[test]
    fn example() {
        let max = u64::MAX;
        let conved = max as i64;
        assert_eq!(max, conved as u64)
    }
}
