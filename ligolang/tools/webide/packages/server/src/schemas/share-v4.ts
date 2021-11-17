import joi from '@hapi/joi';

import { Migration } from './migration';
import { SchemaMigrationV3, SchemaV3 } from './share-v3';

export type Version = 'v4';

export interface SchemaV4 {
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
      network: string;
      signer: string;
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

export class SchemaMigrationV4 extends Migration {
  readonly VERSION: Version = 'v4';

  protected readonly schema = joi.object({
    version: joi.string().required().allow(this.VERSION),
    state: joi.object({
      editor: joi
        .object({
          language: joi.string().required(),
          code: joi.string().required(),
          dirty: joi.boolean().optional(),
          title: joi.string().allow(''),
        })
        .required(),
      compile: joi.object({
        entrypoint: joi.string().allow(''),
      }),
      dryRun: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
        storage: joi.any().allow(''),
      }),
      deploy: joi.object({
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        network: joi.string().allow(''),
        signer: joi.string().allow(''),
      }),
      evaluateValue: joi.object({
        entrypoint: joi.string().allow(''),
      }),
      evaluateFunction: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
      }),
      generateDeployScript: joi.object({
        tool: joi.string().allow(''),
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        originationAccount: joi.string().allow(''),
        burnCap: joi.number().allow(''),
      }),
    }),
  });

  protected readonly previous = new SchemaMigrationV3();

  protected migrate(data: SchemaV3): SchemaV4 {
    return {
      ...data,
      version: this.VERSION,
      state: {
        ...data.state,
        deploy: {
          entrypoint: '',
          storage: '',
          network: '',
          signer: '',
        },
      },
    };
  }
}
