import joi from '@hapi/joi';

import { Migration } from './migration';
import { SchemaMigrationV1, SchemaV1 } from './share-v1';

export type Version = 'v2';

export interface SchemaV2 {
  version: Version;
  state: {
    editor: {
      language: string;
      code: string;
      dirty: boolean;
      title: string;
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

export class SchemaMigrationV2 extends Migration {
  readonly VERSION: Version = 'v2';

  protected readonly schema = joi.object({
    version: joi
      .string()
      .required()
      .allow(this.VERSION),
    state: joi.object({
      editor: joi
        .object({
          language: joi.string().required(),
          code: joi.string().required(),
          dirty: joi.boolean().optional(),
          title: joi.string().allow('')
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

  protected readonly previous = new SchemaMigrationV1();

  protected migrate(data: SchemaV1): SchemaV2 {
    return {
      ...data,
      version: this.VERSION,
      state: {
        ...data.state,
        editor: {
          ...data.state.editor,
          dirty: false,
          title: ''
        }
      }
    };
  }
}
