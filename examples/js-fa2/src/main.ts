import { get, set, main } from 'deku_js_interop';
import { Deku_storage, transition_state, initial_state } from './fa2';

const deku_storage: Deku_storage = {
  get: (key: string) => {
    const result = get(key);
    if (result.toString() === "null") { // TODO: it seems strange
      return undefined
    }
    return result.toString();
  },
  set: (key: string, value: string) =>
    set(key, value)
}

if(process.env.NODE_ENV === 'test') {
  const state = JSON.parse(process.env.TEST_STATE);
  main(state, transition_state(deku_storage));
} else {
  main(initial_state, transition_state(deku_storage));
}
