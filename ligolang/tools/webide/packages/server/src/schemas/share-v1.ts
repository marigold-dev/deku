import joi from '@hapi/joi';

import { Migration } from './migration';
import { SchemaMigrationV0, SchemaV0 } from './share-v0';

export type Version = 'v1';

export interface SchemaV1 {
  version: Version;
  state: {
    editor: {
      language: string;
      code: string;
    };
    compile: {
      entrypoint: string;
    };
    dryRun: {
      entrypoint: string;
      parameters: string;
      storage: string;
    };
    deploy: {
      entrypoint: string;
      storage: string;
      useTezBridge?: boolean;
    };
    evaluateValue: {
      entrypoint: string;
    };
    evaluateFunction: {
      entrypoint: string;
      parameters: string;
    };
  };
}

export class SchemaMigrationV1 extends Migration {
  readonly VERSION: Version = 'v1';

  protected readonly schema = joi.object({
    version: joi
      .string()
      .required()
      .allow(this.VERSION),
    state: joi.object({
      editor: joi
        .object({
          language: joi.string().required(),
          code: joi.string().required()
        })
        .required(),
      compile: joi.object({
        entrypoint: joi.string().allow('')
      }),
      dryRun: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
        storage: joi.any().allow('')
      }),
      deploy: joi.object({
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        useTezBridge: joi.boolean().optional()
      }),
      evaluateValue: joi.object({
        entrypoint: joi.string().allow('')
      }),
      evaluateFunction: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow('')
      })
    })
  });

  protected readonly previous = new SchemaMigrationV0();

  protected migrate(data: SchemaV0): SchemaV1 {
    return {
      version: this.VERSION,
      state: {
        editor: {
          language: data.language,
          code: data.code
        },
        compile: {
          entrypoint: data.entrypoint
        },
        dryRun: {
          entrypoint: data.entrypoint,
          parameters: data.parameters,
          storage: data.storage
        },
        deploy: {
          entrypoint: data.entrypoint,
          storage: data.storage,
          useTezBridge: false
        },
        evaluateValue: {
          entrypoint: data.entrypoint
        },
        evaluateFunction: {
          entrypoint: data.entrypoint,
          parameters: data.parameters
        }
      }
    };
  }
}
