/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

package org.jetbrains.kotlin.backend.konan

import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.konan.file.File
import org.jetbrains.kotlin.konan.target.CompilerOutputKind
import org.jetbrains.kotlin.konan.target.KonanTarget
import org.jetbrains.kotlin.library.KotlinLibrary
import org.jetbrains.kotlin.library.resolver.KotlinLibraryResolveResult

class CacheSupport(
        configuration: CompilerConfiguration,
        resolvedLibraries: KotlinLibraryResolveResult,
        target: KonanTarget,
        produce: CompilerOutputKind
) {
    private val allLibraries = resolvedLibraries.getFullList()

    // TODO: consider using [FeaturedLibraries.kt].
    private val fileToLibrary = allLibraries.associateBy { it.libraryFile }

    internal val cachedLibraries: CachedLibraries = run {
        val explicitCacheFiles = configuration.get(KonanConfigKeys.CACHED_LIBRARIES)!!

        val explicitCaches = explicitCacheFiles.entries.associate { (libraryPath, cachePath) ->
            val library = fileToLibrary[File(libraryPath)]
                    ?: configuration.reportCompilationError("cache not applied: library $libraryPath in $cachePath")

            library to cachePath
        }

        val implicitCacheDirectories = configuration.get(KonanConfigKeys.CACHE_DIRECTORIES)!!
                .map {
                    File(it).takeIf { it.isDirectory }
                            ?: configuration.reportCompilationError("cache directory $it is not found or not a directory")
                }

        CachedLibraries(
                target = target,
                allLibraries = allLibraries,
                explicitCaches = explicitCaches,
                implicitCacheDirectories = implicitCacheDirectories
        )
    }

    private fun getLibrary(file: File) =
            fileToLibrary[file] ?: error("library to cache\n" +
                    "  ${file.absolutePath}\n" +
                    "not found among resolved libraries:\n  " +
                    allLibraries.joinToString("\n  ") { it.libraryFile.absolutePath })

    internal val librariesToCache: Set<KotlinLibrary> = run {
        val libraryToAddToCachePath = configuration.get(KonanConfigKeys.LIBRARY_TO_ADD_TO_CACHE)
        if (libraryToAddToCachePath.isNullOrEmpty()) {
            configuration.get(KonanConfigKeys.LIBRARIES_TO_CACHE)!!
                    .map { getLibrary(File(it)) }
                    .toSet()
                    .also { if (!produce.isCache) check(it.isEmpty()) }
        } else {
            val libraryToAddToCacheFile = File(libraryToAddToCachePath)
            val libraryToAddToCache = getLibrary(libraryToAddToCacheFile)
            val libraryCache = cachedLibraries.getLibraryCache(libraryToAddToCache)
            if (libraryCache == null)
                setOf(libraryToAddToCache)
            else
                emptySet()
        }
    }

    init {
        // Ensure dependencies of every cached library are cached too:
        resolvedLibraries.getFullList { libraries ->
            libraries.map { library ->
                val cache = cachedLibraries.getLibraryCache(library.library)
                if (cache != null || library.library in librariesToCache) {
                    library.resolvedDependencies.forEach {
                        if (!cachedLibraries.isLibraryCached(it.library) && it.library !in librariesToCache) {
                            val description = if (cache != null) {
                                "cached (in ${cache.path})"
                            } else {
                                "going to be cached"
                            }
                            configuration.reportCompilationError(
                                    "${library.library.libraryName} is $description, " +
                                            "but its dependency isn't: ${it.library.libraryName}"
                            )
                        }
                    }
                }

                library
            }
        }

        // Ensure not making cache for libraries that are already cached:
        librariesToCache.forEach {
            val cache = cachedLibraries.getLibraryCache(it)
            if (cache != null) {
                configuration.reportCompilationError("Can't cache library '${it.libraryName}' " +
                        "that is already cached in '${cache.path}'")
            }
        }
    }
}