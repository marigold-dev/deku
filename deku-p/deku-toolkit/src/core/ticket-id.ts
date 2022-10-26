export type TicketID = {
  ticketer: string;
  data: string;
};

const createTicketID = (ticketer: string, data: string): TicketID => {
  return {
    ticketer,
    data,
  };
};

export default {
  createTicketID,
};
