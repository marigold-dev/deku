import latestSchema from '../../src/schemas/share-latest';

describe('Latest Share Schema Migration', () => {
  it('should be v4', () => {
    expect(latestSchema.VERSION).toEqual('v4');
  });
});
