import { SchemaMigrationV1 } from '../../src/schemas/share-v1';

const V0 = {
  language: 'language',
  code: 'code',
  entrypoint: 'entrypoint',
  parameters: 'parameters',
  storage: 'storage'
};

const V1 = {
  version: 'v1',
  state: {
    editor: {
      language: 'language',
      code: 'code'
    },
    compile: {
      entrypoint: 'entrypoint'
    },
    dryRun: {
      entrypoint: 'entrypoint',
      parameters: 'parameters',
      storage: 'storage'
    },
    deploy: {
      entrypoint: 'entrypoint',
      storage: 'storage',
      useTezBridge: false
    },
    evaluateFunction: {
      entrypoint: 'entrypoint',
      parameters: 'parameters'
    },
    evaluateValue: {
      entrypoint: 'entrypoint'
    }
  }
};

describe('Share Schema Migration V1', () => {
  it('should forward migrate v0 to v1', () => {
    expect(new SchemaMigrationV1().forward(V0)).toEqual(V1);
  });

  it('should forward migrate v1 to v1', () => {
    expect(new SchemaMigrationV1().forward(V1)).toEqual(V1);
  });

  it('should throw error on unknown version', () => {
    function forwardUnknownVersion() {
      new SchemaMigrationV1().forward({
        a: 0
      });
    }

    expect(forwardUnknownVersion).toThrowError();
  });
});
