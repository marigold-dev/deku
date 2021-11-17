import fs from 'fs';
import path from 'path';
import tmp from 'tmp';

import { logger } from './logger';

const { spawn } = require('child_process');
const dataDir = process.env['DATA_DIR'] || path.join(__dirname, 'tmp');
// non existing tmp folder triggered a 500 error
if (!fs.existsSync(dataDir)) {
  fs.mkdirSync(dataDir);
}

const JOB_TIMEOUT = 50000;

export class CompilerError extends Error {
  constructor(message: string) {
    super(message);
  }
}

export class LigoCompiler {
  private ligoCmd = process.env['LIGO_CMD'] || [
    'docker',
    'run',
    '-t',
    '--rm',
    '-v',
    `${dataDir}:${dataDir}`,
    '-w',
    dataDir,
    'ligolang/ligo:next',
  ];

  private execPromise(cmd: string | string[], args: string[]): Promise<string> {
    let command: string[] = [];
    if (Array.isArray(cmd)) {
      command = cmd;
    } else {
      command = cmd.split(' ');
    }

    let program = command[0];
    const argument = [...command.slice(1), ...args];

    return new Promise((resolve, reject) => {
      try {
        const result = spawn(program, argument, { shell: false, cwd: dataDir });
        let finalResult = '';
        let finalError = '';

        result.stdout.on('data', (data: Buffer) => {
          finalResult += data.toString();
        });

        result.stderr.on('data', (data: Buffer) => {
          finalError += data.toString();
        });

        result.on('close', (code: any) => {
          if (code === 0) {
            resolve(finalResult);
          } else {
            reject(new CompilerError(finalError));
          }
        });
      } catch (ex) {
        logger.error(`Unexpected compiler error ${ex}`);
        reject(ex);
      }

      setTimeout(() => {
        reject(new Error(`command: ${cmd} Timed out after ${JOB_TIMEOUT} ms`));
      }, JOB_TIMEOUT);
    });
  }

  private createTemporaryFile(fileContent: string) {
    return new Promise<{ name: string; remove: () => void }>(
      (resolve, reject) => {
        tmp.file(
          { dir: dataDir, postfix: '.ligo' },
          (err, name, fd, remove) => {
            if (err) {
              reject(err);
              return;
            }

            fs.write(fd, Buffer.from(fileContent), (err) => {
              if (err) {
                reject(err);
                return;
              }

              resolve({
                name,
                remove: () => {
                  try {
                    remove();
                  } catch (ex) {
                    logger.error(`Unable to remove file ${name}`);
                  }
                  const ppFile = name.replace('.ligo', '.pp.ligo');
                  try {
                    if (fs.existsSync(ppFile)) {
                      fs.unlinkSync(ppFile);
                    }
                  } catch (ex) {
                    logger.error(`Unable to remove file ${ppFile}`);
                  }
                },
              });
            });
          }
        );
      }
    );
  }

  async compileContract(
    syntax: string,
    code: string,
    entrypoint: string,
    format: string
  ) {
    const { name, remove } = await this.createTemporaryFile(code);

    try {
      const result = await this.execPromise(this.ligoCmd, [
        'compile', 
        'contract',
        name,
        '-e',
        entrypoint,
        '--michelson-format',
        format,
        '-s',
        syntax,
      ]);
      return result;
    } finally {
      remove();
    }
  }

  async compileExpression(
    syntax: string,
    expression: string,
    methodName: string
  ) {
    const { name, remove } = await this.createTemporaryFile(expression);
    try {
      const result = await this.execPromise(this.ligoCmd, [
        'compile',
        'expression',
        syntax,
        methodName,
        '--init-file',
        name,
      ]);

      return result;
    } finally {
      remove();
    }
  }

  async listDeclaration(syntax: string, code: string) {
    const { name, remove } = await this.createTemporaryFile(code);
    try {
      const result = await this.execPromise(this.ligoCmd, [
        'info',
        'list-declarations',
        name,
        '-s',
        syntax,
      ]);
      return result;
    } finally {
      remove();
    }
  }

  async compileStorage(
    syntax: string,
    code: string,
    entrypoint: string,
    format: string,
    storage: string
  ) {
    const { name, remove } = await this.createTemporaryFile(code);

    try {
      const result = await this.execPromise(this.ligoCmd, [
        'compile', 
        'storage',
        name,
        storage,
        '-e',
        entrypoint,
        '--michelson-format',
        format,
        '-s',
        syntax,
      ]);

      return result;
    } finally {
      remove();
    }
  }

  async dryRun(
    syntax: string,
    code: string,
    entrypoint: string,
    parameter: string,
    storage: string
  ) {
    const { name, remove } = await this.createTemporaryFile(code);
    try {
      const result = await this.execPromise(this.ligoCmd, [
        'run',
        'dry-run',
        name,
        parameter,
        storage,
        '-e',
        entrypoint,
        '-s',
        syntax,
      ]);
      return result;
    } finally {
      remove();
    }
  }

  async evaluateValue(syntax: string, code: string, entrypoint: string) {
    const { name, remove } = await this.createTemporaryFile(code);
    try {
      const result = await this.execPromise(this.ligoCmd, [
        'run',
        'evaluate-expr',
        '-s',
        syntax,
        name,
        entrypoint,
      ]);
      return result;
    } finally {
      remove();
    }
  }

  async runFunction(
    syntax: string,
    code: string,
    entrypoint: string,
    parameter: string
  ) {
    const { name, remove } = await this.createTemporaryFile(code);
    try {
      const result = await this.execPromise(this.ligoCmd, [
        'run',
        'evaluate-call',
        name,
        parameter,
        '-e',
        entrypoint,
        '-s',
        syntax,
      ]);
      return result;
    } finally {
      remove();
    }
  }
}
