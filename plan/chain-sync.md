## Scope
 
- Implement chain sync protocol in @src/Hoard/Effects.Network.hs
- Handle ChainSync messages (tip discovery, headers, rollback)
- Update test-connection app to demonstrate ChainSync functionality

## Dependencies

- **#27** - Requires working connection to peers

## Implementation Notes

- Use docs/n2n-pg.hs for reference on ChainSync mini-protocol

## Acceptance Criteria

- [ ] Can establish ChainSync mini-protocol with connected peer
- [ ] Receives and processes chain tips
- [ ] Receives and processes headers
- [ ] Emits `HeaderReceived` events
- [ ] Can identify which blocks need to be fetched
