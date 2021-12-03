import joi from '@hapi/joi';
import { Request, Response } from 'express';

import { CompilerError, LigoCompiler } from '../ligo-compiler';
import { logger } from '../logger';

interface CompileBody {
  syntax: string;
  code: string;
  entrypoint: string;
  format?: string;
}

const validateRequest = (body: any): { value: CompileBody; error?: any } => {
  return joi
    .object({
      syntax: joi.string().required(),
      code: joi.string().required(),
      entrypoint: joi.string().required(),
      format: joi.string().optional(),
    })
    .validate(body);
};

export async function compileContractHandler(req: Request, res: Response) {
  const { error, value: body } = validateRequest(req.body);

  if (error) {
    res.status(400).json({ error: error.message });
  } else {
    try {
      const michelsonCode = await new LigoCompiler().compileContract(
        body.syntax,
        body.code,
        body.entrypoint,
        body.format || 'text'
      );

      res.send({ result: michelsonCode });
    } catch (ex) {
      if (ex instanceof CompilerError) {
        res.status(400).json({ error: ex.message });
      } else {
        logger.error((ex as Error).message);
        res.sendStatus(500);
      }
    }
  }
}
