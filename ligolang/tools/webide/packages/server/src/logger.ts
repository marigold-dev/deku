import { createLogger, format, transports } from 'winston';
const { combine, timestamp, simple } = format;
import expressWinston from 'express-winston';

interface Logger {
  debug: (message: string) => void;
  info: (message: string) => void;
  warn: (message: string) => void;
  error: (message: string) => void;
}

const config = {
  format: combine(timestamp(), simple()),
  transports: [new transports.Console()]
};

export const logger: Logger = createLogger(config);
export const loggerMiddleware = expressWinston.logger({
  ...config,
  msg: 'HTTP {{req.method}} {{req.url}}',
  requestWhitelist: [...expressWinston.requestWhitelist, 'body'],
  responseWhitelist: [...expressWinston.responseWhitelist, 'body']
});
export const errorLoggerMiddleware = expressWinston.errorLogger(config);
