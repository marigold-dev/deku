import joi from '@hapi/joi';

import { Migration } from './migration';
import { SchemaMigrationV2, SchemaV2 } from './share-v2';

export type Version = 'v3';

export interface SchemaV3 {
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
    generateDeployScript: {
      tool: string;
      entrypoint: string;
      storage: string;
      originationAccount: string;
      burnCap: number;
    };
  };
}

export class SchemaMigrationV3 extends Migration {
  readonly VERSION: Version = 'v3';

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
      }),
      generateDeployScript: joi.object({
        tool: joi.string().allow(''),
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        originationAccount: joi.string().allow(''),
        burnCap: joi.number().allow('')
      })
    })
  });

  protected readonly previous = new SchemaMigrationV2();

  protected migrate(data: SchemaV2): SchemaV3 {
    return {
      ...data,
      version: this.VERSION,
      state: {
        ...data.state,
        generateDeployScript: {
          tool: 'tezos-client',
          entrypoint: '',
          storage: '',
          originationAccount: '',
          burnCap: 0
        }
      }
    };
  }
}
