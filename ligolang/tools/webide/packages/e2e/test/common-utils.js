const fetch = require('node-fetch');

//
// Generic utils
//
exports.sleep = (time) => {
  return new Promise((resolve) => setTimeout(resolve, time));
};

exports.clearText = async keyboard => {
  await keyboard.down('Shift');
  for (let i = 0; i < 100; i++) {
    await keyboard.press('ArrowUp');
  }
  await keyboard.up('Shift');
  await keyboard.press('Backspace');
  await keyboard.down('Shift');
  for (let i = 0; i < 100; i++) {
    await keyboard.press('ArrowDown');
  }
  await keyboard.up('Shift');
  await keyboard.press('Backspace');
};

exports.createResponseCallback = (page, url) => {
  return new Promise(resolve => {
    page.on('response', function callback(response) {
      if (response.url() === url) {
        resolve(response);
        page.removeListener('response', callback);
      }
    });
  });
};

exports.getInnerText = id => {
  let element = document.getElementById(id);
  return element && element.textContent;
};

exports.getInputValue = id => {
  let element = document.getElementById(id);
  return element && element.value;
};

//
// Application specific utils
//
exports.API_HOST = process.env['API_HOST'] || 'http://127.0.0.1:8080';
exports.API_ROOT = `${exports.API_HOST}/api`;

exports.fetchExamples = async () => (await fetch(`${exports.API_HOST}/static/examples/list`)).json();

exports.runCommandAndGetOutputFor = async (command, endpoint) => {
  page.on('console', msg => console.log('PAGE LOG:', msg.text()));

  await page.click('#configure-tab');
  await exports.sleep(1000);

  await page.click('#command-select');
  await page.click(`#${command}`);

  // Gotta create response callback before clicking run because some responses are too fast
  const responseCallback = exports.createResponseCallback(page, `${exports.API_ROOT}/${endpoint}`);

  await page.click('#run');
  await responseCallback;

  return page.evaluate(exports.getInnerText, 'output');
};

exports.verifyAllExamples = async (action, done) => {
  const examples = await exports.fetchExamples();

  for (example of examples) {
    await page.click(`#${example.id}`);

    expect(await action()).not.toContain('Error: ');
  }

  done();
};

exports.verifyWithParameter = async (command, parameter, value, action, done) => {
  await page.click('#command-select');
  await page.click(`#${command}`);

  await page.click(`#${parameter}`);
  await exports.clearText(page.keyboard);
  await page.keyboard.type(value);

  expect(await action()).toEqual(`Error: "${parameter}" is not allowed to be empty`);

  done();
}

exports.verifyWithBlankParameter = async (command, parameter, action, done) => {
  await page.click('#command-select');
  await page.click(`#${command}`);

  await page.click(`#${parameter}`);
  await exports.clearText(page.keyboard);

  expect(await action()).toEqual(`Error: "${parameter}" is not allowed to be empty`);

  done();
}

exports.verifyEntrypointBlank = async (command, action, done) => {
  exports.verifyWithBlankParameter(command, 'entrypoint', action, done);
}

exports.verifyParametersBlank = async (command, action, done) => {
  exports.verifyWithBlankParameter(command, 'parameters', action, done);
}

exports.verifyStorageBlank = async (command, action, done) => {
  exports.verifyWithBlankParameter(command, 'storage', action, done);
}

exports.verifyWithCompilationError = async (action, done) => {
  await page.click('#editor');
  await page.keyboard.type('asdf');

  expect(await action()).toContain('Error: ');
  done();
};
