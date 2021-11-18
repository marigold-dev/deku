import { SchemaMigrationV0 } from '../../src/schemas/share-v0';

const V0 = {
  language: 'language',
  code: 'code',
  entrypoint: 'entrypoint',
  parameters: 'parameters',
  storage: 'storage'
};

describe('Share Schema Migration V0', () => {
  it('should forward migrate v0 to v0', () => {
    expect(new SchemaMigrationV0().forward(V0)).toEqual(V0);
  });

  it('should throw error on unknown version', () => {
    function forwardMigrateUnknownVersion() {
      new SchemaMigrationV0().forward({
        a: 0
      });
    }

    expect(forwardMigrateUnknownVersion).toThrowError();
  });
});
