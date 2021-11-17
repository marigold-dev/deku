import joi from '@hapi/joi';

import { Migration } from './migration';

export interface SchemaV0 {
  code: string;
  language: string;
  entrypoint: string;
  parameters: string;
  storage: string;
}

export class SchemaMigrationV0 extends Migration {
  protected readonly schema = joi.object({
    code: joi.string().required(),
    language: joi.string().required(),
    entrypoint: joi.string().required(),
    parameters: joi.any().required(),
    storage: joi.any().required()
  });

  protected readonly previous: Migration | null = null;

  protected migrate(_: any): any {
    throw new Error(
      'Called migrate() on the first migration. Cannot migrate v0 -> v0.'
    );
  }
}
