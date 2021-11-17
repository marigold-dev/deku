const createHash = require('crypto').createHash;
const glob = require('glob');
const join = require('path').join;
const fs = require('fs');
const YAML = require('yamljs');


function urlFriendlyHash(content) {
  const hash = createHash('md5');
  hash.update(content);

  return hash
    .digest('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
}

function convertToJson(content, path) {
  const METADATA_REGEX = /\(\*_\*([^]*?)\*_\*\)\s*/;
  const match = content.match(METADATA_REGEX);

  if (!match || !match[1]) {
    throw new Error(`Unable to find compiler configuration in ${path}.`);
  }

  try {
    const config = YAML.parse(match[1]);
    config.editor = {
      language: config.language,
      code: content.replace(METADATA_REGEX, '')
    };
    delete config.language;

    return config;
  } catch (ex) {
    throw new Error(`${path} doesn't contain valid metadata. ${ex}`);
  }
}

function findFiles(pattern, dir) {
  return new Promise((resolve, reject) => {
    glob(pattern, { cwd: dir }, (error, files) => {
      if (error) {
        reject(error);
      } else {
        resolve(files);
      }
    });
  });
}

function readFile(path) {
  return new Promise((resolve, reject) => {
    fs.readFile(path, 'utf8', (error, content) => {
      if (error) {
        reject(error);
      } else {
        resolve(content);
      }
    });
  });
}

function writeFile(path, config) {
  return new Promise((resolve, reject) => {
    fs.writeFile(path, JSON.stringify(config), error => {
      if (error) {
        reject(error);
      } else {
        resolve();
      }
    });
  });
}

async function processExample(srcDir, file, destDir) {
  const path = join(srcDir, file);

  console.log(`Processing ${path}`);

  const content = await readFile(path);
  const config = convertToJson(content, path);
  const id = urlFriendlyHash(file);

  config.id = id;

  await writeFile(join(destDir, id), config);

  return { id: id, name: config.name };
}

function processExamples(srcDir, files, destDir) {
  return Promise.all(files.map(file => processExample(srcDir, file, destDir)));
}

async function main() {
  process.on('unhandledRejection', error => {
    throw error;
  });

  const EXAMPLES_DIR = process.env['EXAMPLES_DIR'] || join(process.cwd(), '../../../../src/test/examples');

  // const EXAMPLES_GLOB = '**/*.ligo';
  // const files = await findFiles(EXAMPLES_GLOB, EXAMPLES_DIR);
 
  const CURATED_EXAMPLES = [
    'pascaligo/arithmetic-contract.ligo',
    'cameligo/arithmetic-contract.ligo',
    'reasonligo/arithmetic-contract.ligo',
    'cameligo/id.mligo',
    'pascaligo/id.ligo',
    'reasonligo/id.religo',
    'cameligo/hashlock.mligo',
    'pascaligo/hashlock.ligo',
    'reasonligo/hashlock.religo'
  ];


  const EXAMPLES_DEST_DIR = join(process.cwd(), 'build', 'static', 'examples');
  fs.mkdirSync(EXAMPLES_DEST_DIR, { recursive: true });

  const examples = await processExamples(
    EXAMPLES_DIR,
    CURATED_EXAMPLES,
    EXAMPLES_DEST_DIR
  );

  const EXAMPLES_LIST_FILE = 'list';
  await writeFile(join(EXAMPLES_DEST_DIR, EXAMPLES_LIST_FILE), examples);
}

main();
