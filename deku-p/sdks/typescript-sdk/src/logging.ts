const DEBUG_LOGGING = Boolean(process.env.DEKU_VM_DEBUG_LOGGING);

const log = (...message) => {
  if (DEBUG_LOGGING)
    console.log("[\x1b[32m%s\x1b[0m] %s", "deku-vm", ...message);
};

export { log };
