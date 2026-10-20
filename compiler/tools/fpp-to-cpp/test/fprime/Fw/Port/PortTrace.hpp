#ifndef FW_PORT_PORTTRACE_HPP
#define FW_PORT_PORTTRACE_HPP

#include <Fw/Comp/PassiveComponentBase.hpp>
#include <Fw/FPrimeBasicTypes.hpp>

namespace Fw {

struct TraceContext {
    U8 traceId[16];
    U8 spanId[8];
    U8 flags;
};

class PortTrace {
  public:
    static void callBegin(const Fw::PassiveComponentBase& caller,
                          FwIndexType portId,
                          FwIndexType portNum,
                          TraceContext& span);
    static void callEnd(const Fw::PassiveComponentBase& caller,
                        FwIndexType portId,
                        FwIndexType portNum,
                        const TraceContext& span);
    static void current(TraceContext& ctx);
    static void dispatchBegin(const Fw::PassiveComponentBase& comp,
                              FwIndexType portId,
                              FwIndexType portNum,
                              const TraceContext& cause,
                              TraceContext& span);
    static void dispatchEnd(const Fw::PassiveComponentBase& comp,
                            FwIndexType portId,
                            FwIndexType portNum,
                            const TraceContext& span);
    static void dropped(const Fw::PassiveComponentBase& comp,
                        FwIndexType portId,
                        FwIndexType portNum,
                        const TraceContext& cause);
};

}

#endif
