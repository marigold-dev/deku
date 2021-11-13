export interface VersionState {
  revision: string;
  branch: string;
}

const DEFAULT_STATE: VersionState = {
  revision: 'dev',
  branch: 'dev'
};

const version = (state = DEFAULT_STATE): VersionState => {
  return state;
};

export default version