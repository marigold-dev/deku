import { LigoCompiler } from '../src/ligo-compiler';

const PASCALIGO_CODE = `
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
  block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
  block { skip } with a - b

function main (const p : action ; const s : int) :
  (list(operation) * int) is
  block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> add(s, n)
  | Decrement(n) -> subtract(s, n)
  end)
`;

describe('Ligo compiler', () => {
  it('should compile storage', async done => {
    const michelsonCode = await new LigoCompiler().compileStorage(
      'pascaligo',
      PASCALIGO_CODE,
      'main',
      'json',
      '0'
    );

    expect(michelsonCode.trim()).toEqual('{ "int": "0" }');

    done();
  });
});
