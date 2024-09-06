/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <sys/types.h>

namespace chibicpp {

/// Convenience wrappers around some commonly used system calls.  The *NoInt
/// wrappers retry on EINTR.  The *Full wrappers retry on EINTR and also loop
/// until all data is written.  Note that *Full wrappers weaken the thread
/// semantics of underlying system calls.
int open_no_interrupt(char const* name, int flags, mode_t mode = 0666);
}