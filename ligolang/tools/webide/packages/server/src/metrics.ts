const client = require('prom-client');
const gauge = new client.Gauge({
  name: 'ligo_webide_build_info',
  help: 'Ligo Web IDE build info',
  labelNames: ['branch', 'revision']
}).labels(process.env['GIT_TAG'], process.env['GIT_COMMIT']);
gauge.set(1);
