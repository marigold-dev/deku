// @ts-ignore
import { main, get, set, transaction } from "@marigold-dev/deku-p-sdk";
import { cookieBaker } from "./types";
import {
  addCookie,
  addCursor,
  addGrandma,
  addFarm,
  addMine,
  addFactory,
  addBank,
  addTemple,
  addWizard,
  addShipment,
  addAlchemy,
  addPortal,
  addTimeMachine,
  addAntimatter,
  addPrism,
  addChanceMaker,
  addFractal,
  addJavaScript,
  addIdleverse,
  addCordex,
  createEmptyCookieBaker,
  initCookieBaker,
  transferCookies,
  eatCookies,
} from "./state";
import {
  actions,
  isEaten,
  isTransfer,
  operations,
  operationType,
} from "./actions";
import { parseReviver, stringifyReplacer } from "./utils";

const saveState = (source: transaction, cookieBaker: cookieBaker) => {
  const cookieBakerJSON = JSON.stringify(cookieBaker, stringifyReplacer);
  set(source, cookieBakerJSON);
  console.log("Successfully saved cookieBaker");
};

const transition = (tx: transaction) => {
  // source -> tz1 address
  // op_hash / tx_hash => BLAKE2B => resolved as string
  // operation => any
  const source = tx.source;
  const op: string = tx.operation;
  const operation: operations = JSON.parse(op, parseReviver);
  const sourceValue = JSON.parse(get(source), parseReviver);
  let cookieBaker: cookieBaker;
  if (sourceValue === undefined || sourceValue === null) {
    cookieBaker = createEmptyCookieBaker();
  } else {
    cookieBaker = sourceValue;
    cookieBaker = initCookieBaker(cookieBaker);
  }
  if (operation.type === operationType.mint) {
    switch (operation.operation) {
      case actions.cookie: {
        //update state
        cookieBaker = addCookie(cookieBaker, operation.amount);
        console.log("Successfully minted cookie");
        saveState(source, cookieBaker);
        break;
      }
      case actions.cursor: {
        //action successful, update state
        cookieBaker = addCursor(cookieBaker);
        console.log("Successfully minted cursor");
        saveState(source, cookieBaker);
        break;
      }
      case actions.grandma: {
        //action successful, update state
        cookieBaker = addGrandma(cookieBaker);
        console.log("Successfully minted grandma");
        saveState(source, cookieBaker);
        break;
      }
      case actions.farm: {
        //action successful, update state
        cookieBaker = addFarm(cookieBaker);
        console.log("Successfully minted farm");
        saveState(source, cookieBaker);
        break;
      }
      case actions.mine: {
        //action successful, update state
        cookieBaker = addMine(cookieBaker);
        console.log("Successfully minted mine");
        saveState(source, cookieBaker);
        break;
      }
      case actions.factory: {
        //action successful, update state
        cookieBaker = addFactory(cookieBaker);
        console.log("Successfully minted factory");
        saveState(source, cookieBaker);
        break;
      }
      case actions.bank: {
        //action successful, update state
        cookieBaker = addBank(cookieBaker);
        console.log("Successfully minted bank");
        saveState(source, cookieBaker);
        break;
      }
      case actions.temple: {
        //action successful, update state
        cookieBaker = addTemple(cookieBaker);
        console.log("Successfully minted temple");
        saveState(source, cookieBaker);
        break;
      }
      case actions.wizard: {
        //action successful, update state
        cookieBaker = addWizard(cookieBaker);
        console.log("Successfully minted wizard");
        saveState(source, cookieBaker);
        break;
      }
      case actions.shipment: {
        //action successful, update state
        cookieBaker = addShipment(cookieBaker);
        console.log("Successfully minted shipment");
        saveState(source, cookieBaker);
        break;
      }
      case actions.alchemy: {
        //action successful, update state
        cookieBaker = addAlchemy(cookieBaker);
        console.log("Successfully minted alchemy");
        saveState(source, cookieBaker);
        break;
      }
      case actions.portal: {
        //action successful, update state
        cookieBaker = addPortal(cookieBaker);
        console.log("Successfully minted portal");
        saveState(source, cookieBaker);
        break;
      }
      case actions.timeMachine: {
        //action successful, update state
        cookieBaker = addTimeMachine(cookieBaker);
        console.log("Successfully minted time machine");
        saveState(source, cookieBaker);
        break;
      }
      case actions.antimatter: {
        //action successful, update state
        cookieBaker = addAntimatter(cookieBaker);
        console.log("Successfully minted antimatter");
        saveState(source, cookieBaker);
        break;
      }
      case actions.prism: {
        //action successful, update state
        cookieBaker = addPrism(cookieBaker);
        console.log("Successfully minted prism");
        saveState(source, cookieBaker);
        break;
      }
      case actions.chanceMaker: {
        //action successful, update state
        cookieBaker = addChanceMaker(cookieBaker);
        console.log("Successfully minted chance maker");
        saveState(source, cookieBaker);
        break;
      }
      case actions.fractal: {
        //action successful, update state
        cookieBaker = addFractal(cookieBaker);
        console.log("Successfully minted fractal");
        saveState(source, cookieBaker);
        break;
      }
      case actions.javaScript: {
        //action successful, update state
        cookieBaker = addJavaScript(cookieBaker);
        console.log("Successfully minted javascript");
        saveState(source, cookieBaker);
        break;
      }
      case actions.idleverse: {
        //action successful, update state
        cookieBaker = addIdleverse(cookieBaker);
        console.log("Successfully minted idleverse");
        saveState(source, cookieBaker);
        break;
      }
      case actions.cordex: {
        //action successful, update state
        cookieBaker = addCordex(cookieBaker);
        console.log("Successfully minted cordex");
        saveState(source, cookieBaker);
        break;
      }
    }
  } else if (operation.type === operationType.transfer) {
    if (isTransfer(operation.operation)) {
      // make sure the recipient has a started game
      let to = JSON.parse(get(operation.operation.to));
      if (!to) {
        console.log(
          "Impossible to transfer to this user, no started game for: ",
          operation.operation.to
        );
      } else if (operation.operation.to === source) {
        console.log(
          "Impossible to transfer cookies to yourself, doesn't mean anything"
        );
      } else {
        let cookieBakerTo = JSON.parse(to, parseReviver);
        cookieBakerTo = initCookieBaker(cookieBakerTo);
        const { from: cookieBakerSender, to: cookieBakerRecipient } =
          transferCookies(
            cookieBaker,
            cookieBakerTo,
            BigInt(operation.operation.amount)
          );
        saveState(source, cookieBakerSender);
        saveState(operation.operation.to, cookieBakerRecipient);
      }
    } else {
      throw new Error("Impossible case! Expected mint, transfer or eat");
    }
  } else if (operation.type === operationType.eat) {
    if (isEaten(operation.operation)) {
      eatCookies(cookieBaker, BigInt(operation.operation.amount));
      saveState(source, cookieBaker);
    }
  } else {
    throw new Error("Impossible case! Expected mint, transfer or eat");
  }
};

main({}, transition);
