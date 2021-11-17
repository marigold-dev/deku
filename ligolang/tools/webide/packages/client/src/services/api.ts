import axios from 'axios';

import { AppState } from '../redux/app';
import { Language } from '../redux/types';

export async function getExampleList() {
  const response = await axios.get(`/static/examples/list`);
  return response.data;
}

export async function getExample(id: string) {
  const response = await axios.get(`/static/examples/${id}`);
  return response.data;
}

export async function getListDeclaration(syntax: string, code: string) {
  const response = await axios.post('/api/list-declaration', {
    syntax,
    code,
  });
  return response.data;
}

export async function compileContract(
  syntax: Language,
  code: string,
  entrypoint: string,
  format?: string
) {
  const response = await axios.post('/api/compile-contract', {
    syntax,
    code,
    entrypoint,
    format,
  });
  return response.data;
}

export async function compileExpression(
  syntax: Language,
  expression: string,
  format?: string
) {
  const response = await axios.post('/api/compile-expression', {
    syntax,
    expression: `${expression}`,
    format,
  });
  return response.data;
}

export async function compileStorage(
  syntax: Language,
  code: string,
  entrypoint: string,
  storage: string,
  format?: string
) {
  // For whatever reason, storage set by examples is not treated as a string. So we convert it here.
  storage = `${storage}`;

  const response = await axios.post('/api/compile-storage', {
    syntax,
    code,
    entrypoint,
    storage,
    format,
  });
  return response.data;
}

export async function dryRun(
  syntax: Language,
  code: string,
  entrypoint: string,
  parameters: string,
  storage: string
) {
  // For whatever reason, storage set by examples is not treated as a string. So we convert it here.
  storage = `${storage}`;

  const response = await axios.post('/api/dry-run', {
    syntax,
    code,
    entrypoint,
    parameters,
    storage,
  });
  return response.data;
}

export async function getSharedFile(fileHash: string) {
  const response = await axios.get(`/api/share/${fileHash}`);
  return response.data;
}

export async function share({
  editor,
  compile,
  dryRun,
  deploy,
  evaluateValue,
  evaluateFunction,
  generateDeployScript,
}: Partial<AppState>) {
  const params = {
    editor,
    compile,
    dryRun,
    deploy,
    evaluateValue,
    evaluateFunction,
    generateDeployScript,
  };

  // We don't want to store the following configuration
  if (params.editor) {
    delete params.editor.lastEditedTime;
  }
  if (params.compile) {
    delete params.compile.michelsonFormat;
  }
  if (params.editor?.cursorPosition) {
    delete params.editor.cursorPosition;
  }

  const response = await axios.post('/api/share', params);
  return response.data;
}

export async function deploy(
  syntax: Language,
  code: string,
  entrypoint: string,
  storage: string,
  network: string
) {
  // For whatever reason, storage set by examples is not treated as a string. So we convert it here.
  storage = `${storage}`;

  const response = await axios.post('/api/deploy', {
    syntax,
    code,
    entrypoint,
    storage,
    network,
  });
  return response.data;
}

export async function evaluateValue(
  syntax: Language,
  code: string,
  entrypoint: string
) {
  const response = await axios.post('/api/evaluate-expr', {
    syntax,
    code,
    entrypoint,
  });
  return response.data;
}

export async function runFunction(
  syntax: Language,
  code: string,
  entrypoint: string,
  parameters: string
) {
  const response = await axios.post('/api/evaluate-call', {
    syntax,
    code,
    entrypoint,
    parameters,
  });
  return response.data;
}

export function getErrorMessage(ex: any): string {
  if (ex.response && ex.response.data) {
    return ex.response.data.error;
  } else if (ex instanceof Error) {
    return ex.message;
  }

  return JSON.stringify(ex);
}
