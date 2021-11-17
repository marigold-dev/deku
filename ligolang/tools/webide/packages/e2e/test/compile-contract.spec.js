const commonUtils = require('./common-utils');

const API_HOST = commonUtils.API_HOST;

const runCommandAndGetOutputFor = commonUtils.runCommandAndGetOutputFor;

const verifyEntrypointBlank = commonUtils.verifyEntrypointBlank;
const verifyAllExamples = commonUtils.verifyAllExamples;
const verifyWithCompilationError = commonUtils.verifyWithCompilationError;

const COMMAND = 'compile';
const COMMAND_ENDPOINT = 'compile-contract';

async function action() {
  return await runCommandAndGetOutputFor(COMMAND, COMMAND_ENDPOINT);
}

describe('Compile contract', () => {
  beforeAll(() => jest.setTimeout(60000));

  beforeEach(async () => await page.goto(API_HOST));

  it('should compile for each examples', async done => {
    verifyAllExamples(action, done);
  });

  it('should return an error when entrypoint is blank', async done => {
    verifyEntrypointBlank(COMMAND, action, done);
  });

  it('should return an error when code has compilation error', async done => {
    verifyWithCompilationError(action, done);
  });
});
