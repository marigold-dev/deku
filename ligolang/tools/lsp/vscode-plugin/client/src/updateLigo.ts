import { execFileSync } from 'child_process'

import * as axios from 'axios'
import * as path from 'path'
import * as fs from 'fs'
import * as semver from 'semver'
import * as vscode from 'vscode'

/* eslint-disable camelcase */
/**
 * Stripped version of the release type returned by GitLab with fields that are
 * interesting to us.
 */
type Release = {
  name: string
  tag_name: string
  released_at: Date
  assets: {
    links: [
      {
        name: string
        direct_asset_url: string
      }
    ]
  }
}
/* eslint-enable camelcase */

export async function installLigo(ligoPath: string, latest: Release): Promise<void> {
  const asset = latest.assets.links.find((download) => download.name === 'Static Linux binary')
  if (!asset) {
    await vscode.window.showErrorMessage('Could not find a download for a static Linux binary.')
    return
  }

  const fileOptions = {
    mode: 0o755,
    encoding: 'binary' as BufferEncoding,
  }

  vscode.window.withProgress(
    {
      cancellable: true,
      location: vscode.ProgressLocation.Notification,
      title: 'Downloading static Linux binary for LIGO',
    },
    async (progress, cancelToken) => axios.default
      .get(
        asset.direct_asset_url,
        {
          responseType: 'arraybuffer',
          cancelToken: new axios.default.CancelToken(cancelToken.onCancellationRequested),
          onDownloadProgress: (progressEvent) => {
            const increment = Math.round((progressEvent.loaded / progressEvent.total) * 100)
            const newState = {
              increment,
              message: `${increment}%`,
            }
            progress.report(newState)
          },
        },
      )
      .then((res) => {
        if (cancelToken.isCancellationRequested) {
          vscode.window.showInformationMessage('LIGO installation cancelled.')
          return
        }

        // FIXME: Sometimes the installation may fail. This will happen if ligo
        // is currently being used by the extension. The error will be ETXTBSY
        // and indicates that the file is currently busy.
        fs.writeFile(ligoPath, Buffer.from(res.data), fileOptions, (err) => {
          if (err) {
            vscode.window.showErrorMessage(`Could not install LIGO: ${err.message}`)
          } else {
            vscode.window.showInformationMessage(`LIGO installed at: ${path.resolve(ligoPath)}`)
          }
        })
      })
      .catch((err) => {
        if (axios.default.isCancel(err)) {
          vscode.window.showInformationMessage('LIGO download cancelled.')
          return
        }

        vscode.window.showErrorMessage(`Could not download LIGO: ${err.message}`)
      }),
  )
}

async function getLigoReleases(): Promise<Release[] | undefined> {
  // https://stackoverflow.com/a/53126068/10213577
  const ligoGitLabProjectId = 12294987
  const releasesUrl = `https://gitlab.com/api/v4/projects/${ligoGitLabProjectId}/releases/`
  return axios.default
    .get(releasesUrl)
    .then((res) => res.data)
    .catch((err) => {
      vscode.window.showErrorMessage(`Could not fetch LIGO releases: ${err.message}`)
      return undefined
    })
}

async function getLatestLigoRelease(): Promise<Release | undefined> {
  const releases = await getLigoReleases()
  if (!releases || releases.length === 0) {
    return undefined
  }

  return releases[0]
}

function getLigoPath(config: vscode.WorkspaceConfiguration): string | undefined {
  const ligoPath = config.get<string>('ligoLanguageServer.ligoBinaryPath')
  if (ligoPath) {
    return ligoPath
  }

  try {
    vscode.window.showWarningMessage('LIGO binary not found through the configuration for the Visual Studio Code extension. Using PATH.')
    return execFileSync('which', ['ligo']).toString().trim()
  } catch {
    return undefined
  }
}

function openLigoReleases(): Thenable<boolean> {
  return vscode.env.openExternal(vscode.Uri.parse('https://gitlab.com/ligolang/ligo/-/releases'))
    .then((result) => {
      if (!result) {
        vscode.window.showErrorMessage('Failed to open LIGO releases page.')
      }

      return result
    })
}

async function promptLigoUpdate(
  ligoPath: string,
  installedVersionIdentifier: string | number,
): Promise<void> {
  const latestRelease = await getLatestLigoRelease()
  if (!latestRelease) {
    return
  }

  switch (typeof installedVersionIdentifier) {
    // Semantic version
    case 'string':
      if (semver.gte(installedVersionIdentifier, latestRelease.tag_name)) {
        return
      }
      break
    // Date of some rolling release
    case 'number':
      if (new Date(installedVersionIdentifier) >= latestRelease.released_at) {
        return
      }
      break
    default:
      vscode.window.showErrorMessage(`Unknown version: ${installedVersionIdentifier}`)
  }

  const answer = await vscode.window.showInformationMessage(
    'A new LIGO version is available. If you use the static Linux binary, please select "Static Linux Binary", otherwise "Open Downloads".',
    'Static Linux Binary',
    'Open Downloads',
    'Cancel',
  )

  switch (answer) {
    case 'Static Linux Binary':
      installLigo(ligoPath, latestRelease)
      break
    case 'Open Downloads':
      openLigoReleases()
      break
    case 'Cancel':
    default:
      break
  }
}

export default async function updateLigo(): Promise<void> {
  const config = vscode.workspace.getConfiguration()
  /* eslint-disable no-use-before-define */
  return updateLigoImpl(config)
  /* eslint-enable no-use-before-define */
}

async function updateLigoImpl(config: vscode.WorkspaceConfiguration): Promise<void> {
  const ligoPath = getLigoPath(config)

  let data: string
  try {
    if (!ligoPath) {
      throw new Error('Undefined path')
    }

    data = execFileSync(ligoPath, ['--version']).toString().trim()
  } catch (err) {
    const answer = await vscode.window.showInformationMessage(
      `Could not find a LIGO installation on your computer or the installation is invalid: ${err.message}`,
      'Choose path',
      'Download',
      'Cancel',
    )

    switch (answer) {
      case 'Choose path': {
        const uris = await vscode.window.showOpenDialog({ canSelectMany: false })
        if (!uris || uris.length === 0) {
          return
        }

        await config.update(
          'ligoLanguageServer.ligoBinaryPath',
          uris[0].fsPath,
          vscode.ConfigurationTarget.Global,
          true,
        )
        updateLigo()
        return
      }
      case 'Download':
        openLigoReleases()
        return
      case 'Cancel':
      default:
        return
    }
  }

  const semverTest = semver.valid(semver.coerce(data))
  if (semverTest) {
    promptLigoUpdate(ligoPath, semverTest)
    return
  }

  const commitTest = /Rolling release\nCommit SHA: [0-9a-f]{40}\nCommit Date: (.+)/
  const commitDate = commitTest.exec(data)
  if (commitDate && commitDate.length === 2) {
    const date: number = Date.parse(commitDate[1])
    if (Number.isNaN(date)) {
      // Parse failed; not a date.
      return
    }

    promptLigoUpdate(ligoPath, date)
  } else {
    vscode.window.showErrorMessage(`Could not identity the installed LIGO version: ${data}`)
  }
}
