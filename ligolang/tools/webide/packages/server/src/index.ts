
import cors, { CorsOptions } from 'cors';
import express from 'express';
import { dirname, join } from 'path';

import { compileContractHandler } from './handlers/compile-contract';
import { compileExpressionHandler } from './handlers/compile-expression';
import { compileStorageHandler } from './handlers/compile-storage';
import { deployHandler } from './handlers/deploy';
import { dryRunHandler } from './handlers/dry-run';
import { evaluateValueHandler } from './handlers/evaluate-expr';
import { runFunctionHandler } from './handlers/evaluate-call';
import { shareHandler } from './handlers/share';
import { sharedLinkHandler } from './handlers/shared-link';
import { listDeclarationHandler } from './handlers/list-declaration';
import { errorLoggerMiddleware, loggerMiddleware } from './logger';
require('./metrics');

const bodyParser = require('body-parser');
const prometheus = require('express-prometheus-middleware');

const app = express();
const APP_PORT = 8080;

const metrics = express();
const METRICS_PORT = 8081;

const corsOptions: CorsOptions  = {
  origin: [
    'https://ligolang.org',
    'http://localhost:3000',
    'http://localhost:1234',
  ],
  optionsSuccessStatus: 200,
};

const appRootDirectory =
  process.env['STATIC_ASSETS'] ||
  dirname(require.resolve('../../client/package.json'));
const appBundleDirectory = join(appRootDirectory, 'build');

app.use(bodyParser.json());
app.use(loggerMiddleware);
app.use(
  prometheus({
    metricsPath: '/metrics',
    collectDefaultMetrics: true,
    collectDefaultBuckets: true,
    requestDurationBuckets: [0.5, 0.6, 0.7, 1, 10, 20, 30, 60],
    metricsApp: metrics,
  })
);

app.use(express.static(appBundleDirectory));
app.use(cors(corsOptions))

app.get(`/api/share/:hash([0-9a-zA-Z\-\_]+)`, sharedLinkHandler());
app.post('/api/compile-contract', compileContractHandler);
app.post('/api/compile-expression', compileExpressionHandler);
app.post('/api/compile-storage', compileStorageHandler);
app.post('/api/dry-run', dryRunHandler);
app.post('/api/share', shareHandler);
app.post('/api/evaluate-expr', evaluateValueHandler);
app.post('/api/evaluate-call', runFunctionHandler);
app.post('/api/deploy', deployHandler);
app.post('/api/list-declaration', listDeclarationHandler);

app.use(errorLoggerMiddleware);

app.listen(APP_PORT, () => {
  console.log(`API listening on: ${APP_PORT}`);
});

metrics.listen(METRICS_PORT, () => {
  console.log(`Metrics listening on: ${METRICS_PORT}`);
});
