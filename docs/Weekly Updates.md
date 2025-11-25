# Weekly status updates

## 2025-11-24
- Team is Progressing on block fetching and storing/comparing headers; investigating CI failure and IO-based tip query implemented; PR coming soon.
- Mainnet node planned on builder1 for local queries and peer discovery; IT consultation pending.
- Development & Code: Working skeleton approach with MVP mentality to accelerate  feedback; legacy cleanup; avoid broad AppEffects; using channel-based events.
- Next Steps: Victor – CI & PR cleanup; Philipp – finalize effects; Nicholas – setup node machine

## 2025-11-17
- Project Progress : Steady progress despite Cadano APIs and codebase complexity.
- Using a local node and consensus libraries to validate blocks and detect orphaned blocks via the local state query protocol; DB sync not required.
- Hoarding Node Design : Initial implementation assumes collocated nodes; communication via channels which emulate an event system and allow easy transition to distributed setups.
- Blocks stored in the database; background processes handle retrieval.
- Node Discovery & Collectors One thread per peer; threads terminate when peers disconnect and collectors report status and disconnect accordingly.
- Pairing session planned to discuss  orphan block detection and chain sync.

## 2025-11-10
-  Protocol & Implementation : Core protocol is functioning, and the team is now receiving clean data , additional protocols will be enabled  aiming for a network connection demo.
- Peer Handling & Architecture : Implementing peer information persistence in the database and identified an issue where peer sharing terminates other protocol threads. There is an interim fix which is to - keep threads alive by putting them to sleep. A future approach may be developed which is a protocol  that will signal completion instead of terminating the application.
- The team continued refining database effects for better structure and testability.
- Rollbacks & Forks : Clarified that the hoarding node only needs to record rollback events and full rollback handling remains the responsibility of the canonical Cardano node.
- A demo should be planned before christmas
  
## 2025-11-03
- CIP vs Design Document : The team reviewed whether the Data Access Layer deliverable should be submitted as a CIP or a design document.and  they agreed to consult the CIP editors to confirm whether a CIP is premature at this stage. The fallback plan is to deliver a design document for milestone acceptance, as this would still satisfy the intent of the deliverable.
- Milestone & Governance Alignment : if CIP editors recommend a design document instead of a CIP, the TSC and community will still recognize it as meeting the milestone’s spirit.
  We will notify CIP editors of the plan and highlight which parts of the work may evolve into a CIP later.
- The team started working on Implement ChainSync mini-protoco ( [Link](https://github.com/orgs/tweag/projects/80/views/1?pane=issue&itemId=136030737&issue=tweag%7Ccardano-hoarding-node%7C29) ) 
- Finally , the team talked about attending Cardano Summit the 12th of November 

## 2025-10-27
- The team worked on Secrets Management : Secrets handling with sops is mostly complete, with decryption occurring before application startup.
- Design & Architecture: The team aligned on the data eviction strategy: persist everything initially, then remove canonical/valid data in the background while retaining “interesting” invalid data.
- Codebase was reviewed covering the event system, server, database layer (relate), and effects (effectful).
- Testing & CI:The project uses tasty for unit tests and integration tests backed by temporary databases for clean isolation.
- The N2PG prototype, will be used , reviewed its source, and integrate its chain-sync and block-fetch logic into the collectors.

## 2025-10-21
- Design Document Alignment: Christian continued implementing items from the design document, translating CIP feedback into concrete technical updates and identifying components that can progress in parallel with CIP refinements.
- The team worked on Hoarding Collector : https://github.com/orgs/tweag/projects/80/views/1?pane=issue&itemId=135013378&issue=tweag%7Ccardano-hoarding-node%7C4
- Onboarding Follow-up:Victor completed his first full week, gaining familiarity with the codebase and workflow.
- Team Coordination: With Philipp on PTO, responsibilities were redistributed to maintain momentum on CIP updates and technical work. 
- Next Milestone Planning: The team outlined upcoming tasks, including technical spikes and documentation updates, in preparation for the next project checkpoint.

## 2025-10-14
- New Team Member Onboarding Victor has joined the team on the 15th of October to 2025. Philipp Zander will be on PTO and be back to continue his contribution to the project
- Christian will continue leading technical implementation as they are decoupled from CIP and will follow design document

## 2025-10-06
- Once the internal review was completed,  the team pushed the CIP for feedback from auditors : https://github.com/cardano-foundation/CIPs/pull/1106.
- After Receiving structured feedback from auditors and ecosystem reviewers. The team met to implement the feedback into a concrete action plan focusing on strengthening the Hoarding Node’s value proposition for the community and identifying short-term technical deliverables in conformance to agreed milestones. 

## 2025-09-29

 -The team presented the initial milestone of the Cardono Improvement Process (CIP) document, which outlines the project idea, problem, and application architecture. The team will be reviewing the detailed technical components  before submission to Intersect.

 ## 2025-09-22
 - Completed feasibility analysis of using n2n-pg for the Hoarding Node MVP.
 - Prepared recommendations for technical implementation and next steps.
 - Updated Hoarding Node design document with findings from the investigation.
 - Began compiling content for the Cardano Improvement Process (CIP) initial milestone presentation or disscuss the need to present it as design document

 ## 2025-09-15
 - Continued technical investigation of n2n-pg and the draft header/block download tool.
 - Identified potential integration points with Hoarding Node architecture.
 - Documented challenges and limitations observed in the current draft tool implementation.
 - Started outlining steps required to convert the draft into an MVP for internal testing.

## 2025-09-08
 - Team started this task:  Investigate n2n-pg as a possible stepping stone for the hoarding node as part of Hoarding Node design document. It turns there is a draft of a tool for downloading headers via ChainSync and blocks via BlockFetch here. We need to investigate whether this could be turned into an MVP ( https://github.com/tweag/cardano-hoarding-node/issues/2 )
