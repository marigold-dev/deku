import joi from '@hapi/joi';
import { Request, Response } from 'express';

import { CompilerError, LigoCompiler } from '../ligo-compiler';
import { logger } from '../logger';

interface ListDeclarationBody {
  code: string;
  syntax: string;
}

const validateRequest = (
  body: any
): { value: ListDeclarationBody; error?: any } => {
  return joi
    .object({
      code: joi.string().required(),
      syntax: joi.string().required(),
    })
    .validate(body);
};

export async function listDeclarationHandler(req: Request, res: Response) {
  const { error, value: body } = validateRequest(req.body);

  if (error) {
    res.status(400).json({ error: error.message });
  } else {
    try {
      const result = await new LigoCompiler().listDeclaration(
        body.syntax,
        body.code
      );
      const declarations = result
        .substr(result.indexOf(':') + 1, result.length)
        .replace(/(\r\n|\n|\r)/gm, ' ')
        .trim()
        .split(' ');

      res.send({ declarations: declarations });
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
