FLATPAK = $$FLATPAKBUILD

run-test:
	runhaskell Setup configure --user --enable-executable-dynamic \
	    -fbuildDraft -fllvm -fcuda
	runhaskell Setup build

# dist-newstyle/cache/plan.json:
#	cabal new-build --dry-run -f-cuda

flatpak.json:	flatpak.cabal.json dist-newstyle/cache/plan.json
	cabal-flatpak --cabal-install --arch x86_64 --arch i386 $< $@

repo-%:	flatpak-corrected.json
	flatpak-builder --jobs=4 --force-clean --arch=$* --repo=$(FLATPAK)/repository --state-dir=$(FLATPAK)/builder/ $(FLATPAK)/build/patch-image $<
	touch $@

patch-image.%.flatpak:	repo-%
	flatpak build-bundle --arch=$* $(FLATPAK)/repository $@ com.github.thielema.patch-image \
	  --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo

flatpak-all:	patch-image.x86_64.flatpak patch-image.i386.flatpak
