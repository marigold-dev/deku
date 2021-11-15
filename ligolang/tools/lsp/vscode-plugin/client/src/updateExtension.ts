import { ExtensionContext } from 'vscode'

import * as axios from 'axios'
import * as path from 'path'
import * as fs from 'fs'
import * as semver from 'semver'
import * as vscode from 'vscode'

import { extensionId } from './common'

/**
 * Stripped version of the extension query type returned by the VSCode
 * Marketplace with fields that are interesting to us.
 */
type ExtensionQuery = {
  results: [
    {
      extensions: [
        {
          versions: [
            {
              version: string
            }
          ]
        }
      ]
    }
  ]
}

async function postWithTimeout<T>(
  ep: string,
  body: any,
  timeout: number = 15000,
): Promise<axios.AxiosResponse<T> | undefined> {
  const req = {
    headers: {
      'Content-Type': 'application/json',
      'User-Agent': extensionId,
    },
    timeout,
  }
  return axios
    .default
    .post(ep, body, req)
    .catch((err) => {
      vscode.window.showErrorMessage(
        `Could not query the LIGO extension from the Visual Studio Code Marketplace: ${err.message}`,
      )
      return undefined
    })
}

async function getCurrentExtensionVersion(): Promise<string | undefined> {
  const query = {
    filters: [
      {
        criteria: [
          {
            filterType: 7, // Filter by name.
            value: extensionId,
          },
        ],
        pageNumber: 1,
        pageSize: 1,
      },
    ],
    flags: 0x200, // Return only the latest version.
  }

  const queryUrl = 'https://marketplace.visualstudio.com/_apis/public/gallery/extensionquery?api-version=6.1-preview.1'
  return (await postWithTimeout<ExtensionQuery>(queryUrl, query))
    ?.data.results[0]?.extensions[0]?.versions[0]?.version
}

export default async function updateExtension(cxt: ExtensionContext): Promise<void> {
  const extensionPath = path.join(cxt.extensionPath, 'package.json')
  const packageFile = JSON.parse(fs.readFileSync(extensionPath, 'utf8'))

  const installedVersion = packageFile?.version

  if (!installedVersion) {
    return
  }

  const availableVersion = await getCurrentExtensionVersion()

  if (!availableVersion) {
    return
  }

  if (semver.gte(installedVersion, availableVersion)) {
    return
  }

  const answer = await vscode.window.showInformationMessage(
    'A new LIGO extension version is available. Would you like to update?',
    'Update',
    'Cancel',
  )

  switch (answer) {
    case 'Update':
      await vscode.commands.executeCommand(
        'workbench.extensions.installExtension',
        extensionId,
      )
      break
    case 'Cancel':
    default:
      break
  }
}
