/*
 * Copyright (c) 2015, Ali Afroozeh and Anastasia Izmaylova, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.iguana.grammar.condition;

import org.iguana.datadependent.env.GLLEvaluator;
import org.iguana.datadependent.env.IEvaluatorContext;
import org.iguana.grammar.slot.BodyGrammarSlot;
import org.iguana.gss.GSSNode;
import org.iguana.parser.IguanaRuntime;
import org.iguana.result.Result;
import org.iguana.utils.input.Input;


public interface Conditions {

    default <T extends Result> boolean execute(
            Input input,
            BodyGrammarSlot slot,
            GSSNode<T> u,
            int leftExtent,
            int rightExtent,
            IguanaRuntime<T> runtime) {
        return execute(input, slot, u, leftExtent, rightExtent, GLLEvaluator.getDefaultEvaluatorContext(), runtime);
    }

    <T extends Result> boolean execute(
        Input input,
        BodyGrammarSlot slot,
        GSSNode<T> u,
        int leftExtent,
        int rightExtent,
        IEvaluatorContext ctx,
        IguanaRuntime<T> runtime
    );

    default <T extends Result> boolean execute(
            Input input,
            BodyGrammarSlot slot,
            GSSNode<T> u,
            int inputIndex,
            IguanaRuntime<T> runtime) {
        return execute(input, slot, u, inputIndex, GLLEvaluator.getDefaultEvaluatorContext(), runtime);
    }

    default <T extends Result> boolean execute(
            Input input,
            BodyGrammarSlot slot,
            GSSNode<T> u,
            int inputIndex,
            IEvaluatorContext ctx,
            IguanaRuntime<T> runtime) {
        return execute(input, slot, u, inputIndex, inputIndex, ctx, runtime);
    }

}
