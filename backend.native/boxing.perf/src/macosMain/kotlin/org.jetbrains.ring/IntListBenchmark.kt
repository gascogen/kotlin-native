/*
 * Copyright 2010-2017 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.ring

open class IntListBenchmark {
    private var _data: List<Int>? = null
    val data: List<Int>
        get() = _data!!

    init {
        val list = ArrayList<Int>(BENCHMARK_SIZE)
        for (n in intValues(BENCHMARK_SIZE))
            list.add(n)
        _data = list
    }

    //Benchmark
    @CountBoxings
    fun copy(): List<Int> {
        return data.toList()
    }

    //Benchmark
    @CountBoxings
    fun copyManual(): List<Int> {
        val list = ArrayList<Int>(data.size)
        for (item in data) {
            list.add(item)
        }
        return list
    }

    //Benchmark
    @CountBoxings
    fun filterAndCount(): Int {
        return data.filter { filterLoad(it) }.count()
    }

    //Benchmark
    @CountBoxings
    fun filterAndMap(): List<String> {
        return data.filter { filterLoad(it) }.map { mapLoad(it) }
    }

    //Benchmark
    @CountBoxings
    fun filterAndMapManual(): ArrayList<String> {
        val list = ArrayList<String>()
        for (it in data) {
            if (filterLoad(it)) {
                val value = mapLoad(it)
                list.add(value)
            }
        }
        return list
    }

    //Benchmark
    @CountBoxings
    fun filter(): List<Int> {
        return data.filter { filterLoad(it) }
    }

    //Benchmark
    @CountBoxings
    fun filterManual(): List<Int> {
        val list = ArrayList<Int>()
        for (it in data) {
            if (filterLoad(it))
                list.add(it)
        }
        return list
    }

    //Benchmark
    @CountBoxings
    fun countFilteredManual(): Int {
        var count = 0
        for (it in data) {
            if (filterLoad(it))
                count++
        }
        return count
    }

    //Benchmark
    @CountBoxings
    fun countFiltered(): Int {
        return data.count { filterLoad(it) }
    }

    //Benchmark
    @CountBoxings
    fun countFilteredLocal(): Int {
        return data.cnt { filterLoad(it) }
    }

    //Benchmark
    @CountBoxings
    fun reduce(): Int {
        return data.fold(0) { acc, it -> if (filterLoad(it)) acc + 1 else acc }
    }
}