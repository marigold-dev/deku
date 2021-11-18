const commonUtils = require('./common-utils');

const API_HOST = commonUtils.API_HOST;

const runCommandAndGetOutputFor = commonUtils.runCommandAndGetOutputFor;

const verifyAllExamples = commonUtils.verifyAllExamples;
const verifyEntrypointBlank = commonUtils.verifyEntrypointBlank;
const verifyParametersBlank = commonUtils.verifyParametersBlank;
const verifyStorageBlank = commonUtils.verifyStorageBlank;
const verifyWithCompilationError = commonUtils.verifyWithCompilationError;

const COMMAND = 'dry-run';
const COMMAND_ENDPOINT = 'dry-run';

async function action() {
  return await runCommandAndGetOutputFor(COMMAND, COMMAND_ENDPOINT);
}

describe('Dry run contract', () => {
  beforeAll(() => jest.setTimeout(60000));

  beforeEach(async () => await page.goto(API_HOST));

  it('should dry run for examples', async done => {
    verifyAllExamples(action, done);
  });

  it('should return an error when entrypoint is blank', async done => {
    verifyEntrypointBlank(COMMAND, action, done);
  });

  it('should return an error when parameters is blank', async done => {
    verifyParametersBlank(COMMAND, action, done);
  });

  it('should return an error when storage is blank', async done => {
    verifyStorageBlank(COMMAND, action, done);
  });

  it('should return an error when code has compilation error', async done => {
    verifyWithCompilationError(action, done);
  });
});
