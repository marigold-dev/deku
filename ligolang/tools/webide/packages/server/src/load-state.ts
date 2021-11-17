import fs from 'fs';
import { join } from 'path';

function readFile(path: string): Promise<string> {
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

export async function loadDefaultState(appBundleDirectory: string) {
  const examples = await readFile(
    join(appBundleDirectory, 'static', 'examples', 'list')
  );
  const examplesList = JSON.parse(examples);
  const defaultState = {
    version: {
      branch: process.env['GIT_TAG'],
      revision: process.env['GIT_COMMIT']
    },
    compile: {},
    dryRun: {},
    deploy: {},
    evaluateValue: {},
    evaluateFunction: {},
    generateDeployScript: {},
    editor: {
      title: ''
    },
    examples: {
      selected: null,
      list: examplesList
    }
  };

  if (examplesList[0]) {
    const example = await readFile(
      join(appBundleDirectory, 'static', 'examples', examplesList[0].id)
    );
    const defaultExample = JSON.parse(example);

    defaultState.compile = {
      ...defaultState.compile,
      ...defaultExample.compile
    };
    defaultState.dryRun = {
      ...defaultState.dryRun,
      ...defaultExample.dryRun
    };
    defaultState.deploy = {
      ...defaultState.deploy,
      ...defaultExample.deploy
    };
    defaultState.evaluateValue = {
      ...defaultState.evaluateValue,
      ...defaultExample.evaluateValue
    };
    defaultState.evaluateFunction = {
      ...defaultState.evaluateFunction,
      ...defaultExample.evaluateFunction
    };
    defaultState.generateDeployScript = {
      ...defaultState.generateDeployScript,
      ...defaultExample.generateDeployScript
    };
    defaultState.editor = {
      ...defaultState.editor,
      ...defaultExample.editor,
      title: defaultExample.name
    };
  }

  return defaultState;
}
