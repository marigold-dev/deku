import { Request, Response } from 'express';

import { loadDefaultState } from '../load-state';
import { logger } from '../logger';
import latestSchema from '../schemas/share-latest';
import { storage } from '../storage';
import { FileNotFoundError } from '../storage/interface';

export function sharedLinkHandler() {
  return async (req: Request, res: Response) => {
    try {
      const content = await storage.read(`${req.params['hash']}.txt`);

      logger.info(content);

      // const storedState = JSON.parse(content);
      // const migratedState = latestSchema.forward(storedState);

      // const state = {
      //   ...migratedState.state,
      //   share: { link: req.params['hash'] },
      // };

      res.send(JSON.stringify(content));
    } catch (ex) {
      if (ex instanceof FileNotFoundError) {
        res.send(ex);
      } else {
        logger.error((ex as Error).message);
        res.sendStatus(500);
      }
    }
  };
}
