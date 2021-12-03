import joi from '@hapi/joi';
import { Request, Response } from 'express';

import { CompilerError, LigoCompiler } from '../ligo-compiler';
import { logger } from '../logger';

interface RunFunctionBody {
  syntax: string;
  code: string;
  entrypoint: string;
  parameters: string;
}

const validateRequest = (body: any): { value: RunFunctionBody; error?: any } => {
  return joi
    .object({
      syntax: joi.string().required(),
      code: joi.string().required(),
      entrypoint: joi.string().required(),
      parameters: joi.string().required()
    })
    .validate(body);
};

export async function runFunctionHandler(req: Request, res: Response) {
  const { error, value: body } = validateRequest(req.body);

  if (error) {
    res.status(400).json({ error: error.message });
  } else {
    try {
      const output = await new LigoCompiler().runFunction(
        body.syntax,
        body.code,
        body.entrypoint,
        body.parameters
      );

      res.send({ output: output });
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
