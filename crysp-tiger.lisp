;; Copyright (c) 2018 Alexis Megas.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from Crysp without specific prior written permission.
;;
;; CRYSP IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; CRYSP, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defvar s_table_1
  (make-array 256
	      :element-type '(unsigned-byte 64)
	      :initial-contents '(#x02aab17cf7e90c5e
				  #xac424b03e243a8ec
				  #x72cd5be30dd5fcd3
				  #x6d019b93f6f97f3a
				  #xcd9978ffd21f9193
				  #x7573a1c9708029e2
				  #xb164326b922a83c3
				  #x46883eee04915870
				  #xeaace3057103ece6
				  #xc54169b808a3535c
				  #x4ce754918ddec47c
				  #x0aa2f4dfdc0df40c
				  #x10b76f18a74dbefa
				  #xc6ccb6235ad1ab6a
				  #x13726121572fe2ff
				  #x1a488c6f199d921e
				  #x4bc9f9f4da0007ca
				  #x26f5e6f6e85241c7
				  #x859079dbea5947b6
				  #x4f1885c5c99e8c92
				  #xd78e761ea96f864b
				  #x8e36428c52b5c17d
				  #x69cf6827373063c1
				  #xb607c93d9bb4c56e
				  #x7d820e760e76b5ea
				  #x645c9cc6f07fdc42
				  #xbf38a078243342e0
				  #x5f6b343c9d2e7d04
				  #xf2c28aeb600b0ec6
				  #x6c0ed85f7254bcac
				  #x71592281a4db4fe5
				  #x1967fa69ce0fed9f
				  #xfd5293f8b96545db
				  #xc879e9d7f2a7600b
				  #x860248920193194e
				  #xa4f9533b2d9cc0b3
				  #x9053836c15957613
				  #xdb6dcf8afc357bf1
				  #x18beea7a7a370f57
				  #x037117ca50b99066
				  #x6ab30a9774424a35
				  #xf4e92f02e325249b
				  #x7739db07061ccae1
				  #xd8f3b49ceca42a05
				  #xbd56be3f51382f73
				  #x45faed5843b0bb28
				  #x1c813d5c11bf1f83
				  #x8af0e4b6d75fa169
				  #x33ee18a487ad9999
				  #x3c26e8eab1c94410
				  #xb510102bc0a822f9
				  #x141eef310ce6123b
				  #xfc65b90059ddb154
				  #xe0158640c5e0e607
				  #x884e079826c3a3cf
				  #x930d0d9523c535fd
				  #x35638d754e9a2b00
				  #x4085fccf40469dd5
				  #xc4b17ad28be23a4c
				  #xcab2f0fc6a3e6a2e
				  #x2860971a6b943fcd
				  #x3dde6ee212e30446
				  #x6222f32ae01765ae
				  #x5d550bb5478308fe
				  #xa9efa98da0eda22a
				  #xc351a71686c40da7
				  #x1105586d9c867c84
				  #xdcffee85fda22853
				  #xccfbd0262c5eef76
				  #xbaf294cb8990d201
				  #xe69464f52afad975
				  #x94b013afdf133e14
				  #x06a7d1a32823c958
				  #x6f95fe5130f61119
				  #xd92ab34e462c06c0
				  #xed7bde33887c71d2
				  #x79746d6e6518393e
				  #x5ba419385d713329
				  #x7c1ba6b948a97564
				  #x31987c197bfdac67
				  #xde6c23c44b053d02
				  #x581c49fed002d64d
				  #xdd474d6338261571
				  #xaa4546c3e473d062
				  #x928fce349455f860
				  #x48161bbacaab94d9
				  #x63912430770e6f68
				  #x6ec8a5e602c6641c
				  #x87282515337ddd2b
				  #x2cda6b42034b701b
				  #xb03d37c181cb096d
				  #xe108438266c71c6f
				  #x2b3180c7eb51b255
				  #xdf92b82f96c08bbc
				  #x5c68c8c0a632f3ba
				  #x5504cc861c3d0556
				  #xabbfa4e55fb26b8f
				  #x41848b0ab3baceb4
				  #xb334a273aa445d32
				  #xbca696f0a85ad881
				  #x24f6ec65b528d56c
				  #x0ce1512e90f4524a
				  #x4e9dd79d5506d35a
				  #x258905fac6ce9779
				  #x2019295b3e109b33
				  #xf8a9478b73a054cc
				  #x2924f2f934417eb0
				  #x3993357d536d1bc4
				  #x38a81ac21db6ff8b
				  #x47c4fbf17d6016bf
				  #x1e0faadd7667e3f5
				  #x7abcff62938beb96
				  #xa78dad948fc179c9
				  #x8f1f98b72911e50d
				  #x61e48eae27121a91
				  #x4d62f7ad31859808
				  #xeceba345ef5ceaeb
				  #xf5ceb25ebc9684ce
				  #xf633e20cb7f76221
				  #xa32cdf06ab8293e4
				  #x985a202ca5ee2ca4
				  #xcf0b8447cc8a8fb1
				  #x9f765244979859a3
				  #xa8d516b1a1240017
				  #x0bd7ba3ebb5dc726
				  #xe54bca55b86adb39
				  #x1d7a3afd6c478063
				  #x519ec608e7669edd
				  #x0e5715a2d149aa23
				  #x177d4571848ff194
				  #xeeb55f3241014c22
				  #x0f5e5ca13a6e2ec2
				  #x8029927b75f5c361
				  #xad139fabc3d6e436
				  #x0d5df1a94ccf402f
				  #x3e8bd948bea5dfc8
				  #xa5a0d357bd3ff77e
				  #xa2d12e251f74f645
				  #x66fd9e525e81a082
				  #x2e0c90ce7f687a49
				  #xc2e8bcbeba973bc5
				  #x000001bce509745f
				  #x423777bbe6dab3d6
				  #xd1661c7eaef06eb5
				  #xa1781f354daacfd8
				  #x2d11284a2b16affc
				  #xf1fc4f67fa891d1f
				  #x73ecc25dcb920ada
				  #xae610c22c2a12651
				  #x96e0a810d356b78a
				  #x5a9a381f2fe7870f
				  #xd5ad62ede94e5530
				  #xd225e5e8368d1427
				  #x65977b70c7af4631
				  #x99f889b2de39d74f
				  #x233f30bf54e1d143
				  #x9a9675d3d9a63c97
				  #x5470554ff334f9a8
				  #x166acb744a4f5688
				  #x70c74caab2e4aead
				  #xf0d091646f294d12
				  #x57b82a89684031d1
				  #xefd95a5a61be0b6b
				  #x2fbd12e969f2f29a
				  #x9bd37013feff9fe8
				  #x3f9b0404d6085a06
				  #x4940c1f3166cfe15
				  #x09542c4dcdf3defb
				  #xb4c5218385cd5ce3
				  #xc935b7dc4462a641
				  #x3417f8a68ed3b63f
				  #xb80959295b215b40
				  #xf99cdaef3b8c8572
				  #x018c0614f8fcb95d
				  #x1b14accd1a3acdf3
				  #x84d471f200bb732d
				  #xc1a3110e95e8da16
				  #x430a7220bf1a82b8
				  #xb77e090d39df210e
				  #x5ef4bd9f3cd05e9d
				  #x9d4ff6da7e57a444
				  #xda1d60e183d4a5f8
				  #xb287c38417998e47
				  #xfe3edc121bb31886
				  #xc7fe3ccc980ccbef
				  #xe46fb590189bfd03
				  #x3732fd469a4c57dc
				  #x7ef700a07cf1ad65
				  #x59c64468a31d8859
				  #x762fb0b4d45b61f6
				  #x155baed099047718
				  #x68755e4c3d50baa6
				  #xe9214e7f22d8b4df
				  #x2addbf532eac95f4
				  #x32ae3909b4bd0109
				  #x834df537b08e3450
				  #xfa209da84220728d
				  #x9e691d9b9efe23f7
				  #x0446d288c4ae8d7f
				  #x7b4cc524e169785b
				  #x21d87f0135ca1385
				  #xcebb400f137b8aa5
				  #x272e2b66580796be
				  #x3612264125c2b0de
				  #x057702bdad1efbb2
				  #xd4babb8eacf84be9
				  #x91583139641bc67b
				  #x8bdc2de08036e024
				  #x603c8156f49f68ed
				  #xf7d236f7dbef5111
				  #x9727c4598ad21e80
				  #xa08a0896670a5fd7
				  #xcb4a8f4309eba9cb
				  #x81af564b0f7036a1
				  #xc0b99aa778199abd
				  #x959f1ec83fc8e952
				  #x8c505077794a81b9
				  #x3acaaf8f056338f0
				  #x07b43f50627a6778
				  #x4a44ab49f5eccc77
				  #x3bc3d6e4b679ee98
				  #x9cc0d4d1cf14108c
				  #x4406c00b206bc8a0
				  #x82a18854c8d72d89
				  #x67e366b35c3c432c
				  #xb923dd61102b37f2
				  #x56ab2779d884271d
				  #xbe83e1b0ff1525af
				  #xfb7c65d4217e49a9
				  #x6bdbe0e76d48e7d4
				  #x08df828745d9179e
				  #x22ea6a9add53bd34
				  #xe36e141c5622200a
				  #x7f805d1b8cb750ee
				  #xafe5c7a59f58e837
				  #xe27f996a4fb1c23c
				  #xd3867dfb0775f0d0
				  #xd0e673de6e88891a
				  #x123aeb9eafb86c25
				  #x30f1d5d5c145b895
				  #xbb434a2dee7269e7
				  #x78cb67ecf931fa38
				  #xf33b0372323bbf9c
				  #x52d66336fb279c74
				  #x505f33ac0afb4eaa
				  #xe8a5cd99a2cce187
				  #x534974801e2d30bb
				  #x8d2d5711d5876d90
				  #x1f1a412891bc038e
				  #xd6e2e71d82e56648
				  #x74036c3a497732b7
				  #x89b67ed96361f5ab
				  #xffed95d8f1ea02a2
				  #xe72b3bd61464d43d
				  #xa6300f170bdc4820
				  #xebc18760ed78a77a)))

(defvar s_table_2
  (make-array 256
	      :element-type '(unsigned-byte 64)
	      :initial-contents '(#xe6a6be5a05a12138
				  #xb5a122a5b4f87c98
				  #x563c6089140b6990
				  #x4c46cb2e391f5dd5
				  #xd932addbc9b79434
				  #x08ea70e42015aff5
				  #xd765a6673e478cf1
				  #xc4fb757eab278d99
				  #xdf11c6862d6e0692
				  #xddeb84f10d7f3b16
				  #x6f2ef604a665ea04
				  #x4a8e0f0ff0e0dfb3
				  #xa5edeef83dbcba51
				  #xfc4f0a2a0ea4371e
				  #xe83e1da85cb38429
				  #xdc8ff882ba1b1ce2
				  #xcd45505e8353e80d
				  #x18d19a00d4db0717
				  #x34a0cfeda5f38101
				  #x0be77e518887caf2
				  #x1e341438b3c45136
				  #xe05797f49089ccf9
				  #xffd23f9df2591d14
				  #x543dda228595c5cd
				  #x661f81fd99052a33
				  #x8736e641db0f7b76
				  #x15227725418e5307
				  #xe25f7f46162eb2fa
				  #x48a8b2126c13d9fe
				  #xafdc541792e76eea
				  #x03d912bfc6d1898f
				  #x31b1aafa1b83f51b
				  #xf1ac2796e42ab7d9
				  #x40a3a7d7fcd2ebac
				  #x1056136d0afbbcc5
				  #x7889e1dd9a6d0c85
				  #xd33525782a7974aa
				  #xa7e25d09078ac09b
				  #xbd4138b3eac6edd0
				  #x920abfbe71eb9e70
				  #xa2a5d0f54fc2625c
				  #xc054e36b0b1290a3
				  #xf6dd59ff62fe932b
				  #x3537354511a8ac7d
				  #xca845e9172fadcd4
				  #x84f82b60329d20dc
				  #x79c62ce1cd672f18
				  #x8b09a2add124642c
				  #xd0c1e96a19d9e726
				  #x5a786a9b4ba9500c
				  #x0e020336634c43f3
				  #xc17b474aeb66d822
				  #x6a731ae3ec9baac2
				  #x8226667ae0840258
				  #x67d4567691caeca5
				  #x1d94155c4875adb5
				  #x6d00fd985b813fdf
				  #x51286efcb774cd06
				  #x5e8834471fa744af
				  #xf72ca0aee761ae2e
				  #xbe40e4cdaee8e09a
				  #xe9970bbb5118f665
				  #x726e4beb33df1964
				  #x703b000729199762
				  #x4631d816f5ef30a7
				  #xb880b5b51504a6be
				  #x641793c37ed84b6c
				  #x7b21ed77f6e97d96
				  #x776306312ef96b73
				  #xae528948e86ff3f4
				  #x53dbd7f286a3f8f8
				  #x16cadce74cfc1063
				  #x005c19bdfa52c6dd
				  #x68868f5d64d46ad3
				  #x3a9d512ccf1e186a
				  #x367e62c2385660ae
				  #xe359e7ea77dcb1d7
				  #x526c0773749abe6e
				  #x735ae5f9d09f734b
				  #x493fc7cc8a558ba8
				  #xb0b9c1533041ab45
				  #x321958ba470a59bd
				  #x852db00b5f46c393
				  #x91209b2bd336b0e5
				  #x6e604f7d659ef19f
				  #xb99a8ae2782ccb24
				  #xccf52ab6c814c4c7
				  #x4727d9afbe11727b
				  #x7e950d0c0121b34d
				  #x756f435670ad471f
				  #xf5add442615a6849
				  #x4e87e09980b9957a
				  #x2acfa1df50aee355
				  #xd898263afd2fd556
				  #xc8f4924dd80c8fd6
				  #xcf99ca3d754a173a
				  #xfe477bacaf91bf3c
				  #xed5371f6d690c12d
				  #x831a5c285e687094
				  #xc5d3c90a3708a0a4
				  #x0f7f903717d06580
				  #x19f9bb13b8fdf27f
				  #xb1bd6f1b4d502843
				  #x1c761ba38fff4012
				  #x0d1530c4e2e21f3b
				  #x8943ce69a7372c8a
				  #xe5184e11feb5ce66
				  #x618bdb80bd736621
				  #x7d29bad68b574d0b
				  #x81bb613e25e6fe5b
				  #x071c9c10bc07913f
				  #xc7beeb7909ac2d97
				  #xc3e58d353bc5d757
				  #xeb017892f38f61e8
				  #xd4effb9c9b1cc21a
				  #x99727d26f494f7ab
				  #xa3e063a2956b3e03
				  #x9d4a8b9a4aa09c30
				  #x3f6ab7d500090fb4
				  #x9cc0f2a057268ac0
				  #x3dee9d2dedbf42d1
				  #x330f49c87960a972
				  #xc6b2720287421b41
				  #x0ac59ec07c00369c
				  #xef4eac49cb353425
				  #xf450244eef0129d8
				  #x8acc46e5caf4deb6
				  #x2ffeab63989263f7
				  #x8f7cb9fe5d7a4578
				  #x5bd8f7644e634635
				  #x427a7315bf2dc900
				  #x17d0c4aa2125261c
				  #x3992486c93518e50
				  #xb4cbfee0a2d7d4c3
				  #x7c75d6202c5ddd8d
				  #xdbc295d8e35b6c61
				  #x60b369d302032b19
				  #xce42685fdce44132
				  #x06f3ddb9ddf65610
				  #x8ea4d21db5e148f0
				  #x20b0fce62fcd496f
				  #x2c1b912358b0ee31
				  #xb28317b818f5a308
				  #xa89c1e189ca6d2cf
				  #x0c6b18576aaadbc8
				  #xb65deaa91299fae3
				  #xfb2b794b7f1027e7
				  #x04e4317f443b5beb
				  #x4b852d325939d0a6
				  #xd5ae6beefb207ffc
				  #x309682b281c7d374
				  #xbae309a194c3b475
				  #x8cc3f97b13b49f05
				  #x98a9422ff8293967
				  #x244b16b01076ff7c
				  #xf8bf571c663d67ee
				  #x1f0d6758eee30da1
				  #xc9b611d97adeb9b7
				  #xb7afd5887b6c57a2
				  #x6290ae846b984fe1
				  #x94df4cdeacc1a5fd
				  #x058a5bd1c5483aff
				  #x63166cc142ba3c37
				  #x8db8526eb2f76f40
				  #xe10880036f0d6d4e
				  #x9e0523c9971d311d
				  #x45ec2824cc7cd691
				  #x575b8359e62382c9
				  #xfa9e400dc4889995
				  #xd1823ecb45721568
				  #xdafd983b8206082f
				  #xaa7d29082386a8cb
				  #x269fcd4403b87588
				  #x1b91f5f728bdd1e0
				  #xe4669f39040201f6
				  #x7a1d7c218cf04ade
				  #x65623c29d79ce5ce
				  #x2368449096c00bb1
				  #xab9bf1879da503ba
				  #xbc23ecb1a458058e
				  #x9a58df01bb401ecc
				  #xa070e868a85f143d
				  #x4ff188307df2239e
				  #x14d565b41a641183
				  #xee13337452701602
				  #x950e3dcf3f285e09
				  #x59930254b9c80953
				  #x3bf299408930da6d
				  #xa955943f53691387
				  #xa15edecaa9cb8784
				  #x29142127352be9a0
				  #x76f0371fff4e7afb
				  #x0239f450274f2228
				  #xbb073af01d5e868b
				  #xbfc80571c10e96c1
				  #xd267088568222e23
				  #x9671a3d48e80b5b0
				  #x55b5d38ae193bb81
				  #x693ae2d0a18b04b8
				  #x5c48b4ecadd5335f
				  #xfd743b194916a1ca
				  #x2577018134be98c4
				  #xe77987e83c54a4ad
				  #x28e11014da33e1b9
				  #x270cc59e226aa213
				  #x71495f756d1a5f60
				  #x9be853fb60afef77
				  #xadc786a7f7443dbf
				  #x0904456173b29a82
				  #x58bc7a66c232bd5e
				  #xf306558c673ac8b2
				  #x41f639c6b6c9772a
				  #x216defe99fda35da
				  #x11640cc71c7be615
				  #x93c43694565c5527
				  #xea038e6246777839
				  #xf9abf3ce5a3e2469
				  #x741e768d0fd312d2
				  #x0144b883ced652c6
				  #xc20b5a5ba33f8552
				  #x1ae69633c3435a9d
				  #x97a28ca4088cfdec
				  #x8824a43c1e96f420
				  #x37612fa66eeea746
				  #x6b4cb165f9cf0e5a
				  #x43aa1c06a0abfb4a
				  #x7f4dc26ff162796b
				  #x6cbacc8e54ed9b0f
				  #xa6b7ffefd2bb253e
				  #x2e25bc95b0a29d4f
				  #x86d6a58bdef1388c
				  #xded74ac576b6f054
				  #x8030bdbc2b45805d
				  #x3c81af70e94d9289
				  #x3eff6dda9e3100db
				  #xb38dc39fdfcc8847
				  #x123885528d17b87e
				  #xf2da0ed240b1b642
				  #x44cefadcd54bf9a9
				  #x1312200e433c7ee6
				  #x9ffcc84f3a78c748
				  #xf0cd1f72248576bb
				  #xec6974053638cfe4
				  #x2ba7b67c0cec4e4c
				  #xac2f4df3e5ce32ed
				  #xcb33d14326ea4c11
				  #xa4e9044cc77e58bc
				  #x5f513293d934fcef
				  #x5dc9645506e55444
				  #x50de418f317de40a
				  #x388cb31a69dde259
				  #x2db4a83455820a86
				  #x9010a91e84711ae9
				  #x4df7f0b7b1498371
				  #xd62a2eabc0977179
				  #x22fac097aa8d5c0e)))

(defvar s_table_3
  (make-array 256
	      :element-type '(unsigned-byte 64)
	      :initial-contents '(#xf49fcc2ff1daf39b
				  #x487fd5c66ff29281
				  #xe8a30667fcdca83f
				  #x2c9b4be3d2fcce63
				  #xda3ff74b93fbbbc2
				  #x2fa165d2fe70ba66
				  #xa103e279970e93d4
				  #xbecdec77b0e45e71
				  #xcfb41e723985e497
				  #xb70aaa025ef75017
				  #xd42309f03840b8e0
				  #x8efc1ad035898579
				  #x96c6920be2b2abc5
				  #x66af4163375a9172
				  #x2174abdcca7127fb
				  #xb33ccea64a72ff41
				  #xf04a4933083066a5
				  #x8d970acdd7289af5
				  #x8f96e8e031c8c25e
				  #xf3fec02276875d47
				  #xec7bf310056190dd
				  #xf5adb0aebb0f1491
				  #x9b50f8850fd58892
				  #x4975488358b74de8
				  #xa3354ff691531c61
				  #x0702bbe481d2c6ee
				  #x89fb24057deded98
				  #xac3075138596e902
				  #x1d2d3580172772ed
				  #xeb738fc28e6bc30d
				  #x5854ef8f63044326
				  #x9e5c52325add3bbe
				  #x90aa53cf325c4623
				  #xc1d24d51349dd067
				  #x2051cfeea69ea624
				  #x13220f0a862e7e4f
				  #xce39399404e04864
				  #xd9c42ca47086fcb7
				  #x685ad2238a03e7cc
				  #x066484b2ab2ff1db
				  #xfe9d5d70efbf79ec
				  #x5b13b9dd9c481854
				  #x15f0d475ed1509ad
				  #x0bebcd060ec79851
				  #xd58c6791183ab7f8
				  #xd1187c5052f3eee4
				  #xc95d1192e54e82ff
				  #x86eea14cb9ac6ca2
				  #x3485beb153677d5d
				  #xdd191d781f8c492a
				  #xf60866baa784ebf9
				  #x518f643ba2d08c74
				  #x8852e956e1087c22
				  #xa768cb8dc410ae8d
				  #x38047726bfec8e1a
				  #xa67738b4cd3b45aa
				  #xad16691cec0dde19
				  #xc6d4319380462e07
				  #xc5a5876d0ba61938
				  #x16b9fa1fa58fd840
				  #x188ab1173ca74f18
				  #xabda2f98c99c021f
				  #x3e0580ab134ae816
				  #x5f3b05b773645abb
				  #x2501a2be5575f2f6
				  #x1b2f74004e7e8ba9
				  #x1cd7580371e8d953
				  #x7f6ed89562764e30
				  #xb15926ff596f003d
				  #x9f65293da8c5d6b9
				  #x6ecef04dd690f84c
				  #x4782275fff33af88
				  #xe41433083f820801
				  #xfd0dfe409a1af9b5
				  #x4325a3342cdb396b
				  #x8ae77e62b301b252
				  #xc36f9e9f6655615a
				  #x85455a2d92d32c09
				  #xf2c7dea949477485
				  #x63cfb4c133a39eba
				  #x83b040cc6ebc5462
				  #x3b9454c8fdb326b0
				  #x56f56a9e87ffd78c
				  #x2dc2940d99f42bc6
				  #x98f7df096b096e2d
				  #x19a6e01e3ad852bf
				  #x42a99ccbdbd4b40b
				  #xa59998af45e9c559
				  #x366295e807d93186
				  #x6b48181bfaa1f773
				  #x1fec57e2157a0a1d
				  #x4667446af6201ad5
				  #xe615ebcacfb0f075
				  #xb8f31f4f68290778
				  #x22713ed6ce22d11e
				  #x3057c1a72ec3c93b
				  #xcb46acc37c3f1f2f
				  #xdbb893fd02aaf50e
				  #x331fd92e600b9fcf
				  #xa498f96148ea3ad6
				  #xa8d8426e8b6a83ea
				  #xa089b274b7735cdc
				  #x87f6b3731e524a11
				  #x118808e5cbc96749
				  #x9906e4c7b19bd394
				  #xafed7f7e9b24a20c
				  #x6509eadeeb3644a7
				  #x6c1ef1d3e8ef0ede
				  #xb9c97d43e9798fb4
				  #xa2f2d784740c28a3
				  #x7b8496476197566f
				  #x7a5be3e6b65f069d
				  #xf96330ed78be6f10
				  #xeee60de77a076a15
				  #x2b4bee4aa08b9bd0
				  #x6a56a63ec7b8894e
				  #x02121359ba34fef4
				  #x4cbf99f8283703fc
				  #x398071350caf30c8
				  #xd0a77a89f017687a
				  #xf1c1a9eb9e423569
				  #x8c7976282dee8199
				  #x5d1737a5dd1f7abd
				  #x4f53433c09a9fa80
				  #xfa8b0c53df7ca1d9
				  #x3fd9dcbc886ccb77
				  #xc040917ca91b4720
				  #x7dd00142f9d1dcdf
				  #x8476fc1d4f387b58
				  #x23f8e7c5f3316503
				  #x032a2244e7e37339
				  #x5c87a5d750f5a74b
				  #x082b4cc43698992e
				  #xdf917becb858f63c
				  #x3270b8fc5bf86dda
				  #x10ae72bb29b5dd76
				  #x576ac94e7700362b
				  #x1ad112dac61efb8f
				  #x691bc30ec5faa427
				  #xff246311cc327143
				  #x3142368e30e53206
				  #x71380e31e02ca396
				  #x958d5c960aad76f1
				  #xf8d6f430c16da536
				  #xc8ffd13f1be7e1d2
				  #x7578ae66004ddbe1
				  #x05833f01067be646
				  #xbb34b5ad3bfe586d
				  #x095f34c9a12b97f0
				  #x247ab64525d60ca8
				  #xdcdbc6f3017477d1
				  #x4a2e14d4decad24d
				  #xbdb5e6d9be0a1eeb
				  #x2a7e70f7794301ab
				  #xdef42d8a270540fd
				  #x01078ec0a34c22c1
				  #xe5de511af4c16387
				  #x7ebb3a52bd9a330a
				  #x77697857aa7d6435
				  #x004e831603ae4c32
				  #xe7a21020ad78e312
				  #x9d41a70c6ab420f2
				  #x28e06c18ea1141e6
				  #xd2b28cbd984f6b28
				  #x26b75f6c446e9d83
				  #xba47568c4d418d7f
				  #xd80badbfe6183d8e
				  #x0e206d7f5f166044
				  #xe258a43911cbca3e
				  #x723a1746b21dc0bc
				  #xc7caa854f5d7cdd3
				  #x7cac32883d261d9c
				  #x7690c26423ba942c
				  #x17e55524478042b8
				  #xe0be477656a2389f
				  #x4d289b5e67ab2da0
				  #x44862b9c8fbbfd31
				  #xb47cc8049d141365
				  #x822c1b362b91c793
				  #x4eb14655fb13dfd8
				  #x1ecbba0714e2a97b
				  #x6143459d5cde5f14
				  #x53a8fbf1d5f0ac89
				  #x97ea04d81c5e5b00
				  #x622181a8d4fdb3f3
				  #xe9bcd341572a1208
				  #x1411258643cce58a
				  #x9144c5fea4c6e0a4
				  #x0d33d06565cf620f
				  #x54a48d489f219ca1
				  #xc43e5eac6d63c821
				  #xa9728b3a72770daf
				  #xd7934e7b20df87ef
				  #xe35503b61a3e86e5
				  #xcae321fbc819d504
				  #x129a50b3ac60bfa6
				  #xcd5e68ea7e9fb6c3
				  #xb01c90199483b1c7
				  #x3de93cd5c295376c
				  #xaed52edf2ab9ad13
				  #x2e60f512c0a07884
				  #xbc3d86a3e36210c9
				  #x35269d9b163951ce
				  #x0c7d6e2ad0cdb5fa
				  #x59e86297d87f5733
				  #x298ef221898db0e7
				  #x55000029d1a5aa7e
				  #x8bc08ae1b5061b45
				  #xc2c31c2b6c92703a
				  #x94cc596baf25ef42
				  #x0a1d73db22540456
				  #x04b6a0f9d9c4179a
				  #xeffdafa2ae3d3c60
				  #xf7c8075bb49496c4
				  #x9cc5c7141d1cd4e3
				  #x78bd1638218e5534
				  #xb2f11568f850246a
				  #xedfabcfa9502bc29
				  #x796ce5f2da23051b
				  #xaae128b0dc93537c
				  #x3a493da0ee4b29ae
				  #xb5df6b2c416895d7
				  #xfcabbd25122d7f37
				  #x70810b58105dc4b1
				  #xe10fdd37f7882a90
				  #x524dcab5518a3f5c
				  #x3c9e85878451255b
				  #x4029828119bd34e2
				  #x74a05b6f5d3ceccb
				  #xb610021542e13eca
				  #x0ff979d12f59e2ac
				  #x6037da27e4f9cc50
				  #x5e92975a0df1847d
				  #xd66de190d3e623fe
				  #x5032d6b87b568048
				  #x9a36b7ce8235216e
				  #x80272a7a24f64b4a
				  #x93efed8b8c6916f7
				  #x37ddbff44cce1555
				  #x4b95db5d4b99bd25
				  #x92d3fda169812fc0
				  #xfb1a4a9a90660bb6
				  #x730c196946a4b9b2
				  #x81e289aa7f49da68
				  #x64669a0f83b1a05f
				  #x27b3ff7d9644f48b
				  #xcc6b615c8db675b3
				  #x674f20b9bcebbe95
				  #x6f31238275655982
				  #x5ae488713e45cf05
				  #xbf619f9954c21157
				  #xeabac46040a8eae9
				  #x454c6fe9f2c0c1cd
				  #x419cf6496412691c
				  #xd3dc3bef265b0f70
				  #x6d0e60f5c3578a9e)))

(defvar s_table_4
  (make-array 256
	      :element-type '(unsigned-byte 64)
	      :initial-contents '(#x5b0e608526323c55
				  #x1a46c1a9fa1b59f5
				  #xa9e245a17c4c8ffa
				  #x65ca5159db2955d7
				  #x05db0a76ce35afc2
				  #x81eac77ea9113d45
				  #x528ef88ab6ac0a0d
				  #xa09ea253597be3ff
				  #x430ddfb3ac48cd56
				  #xc4b3a67af45ce46f
				  #x4ececfd8fbe2d05e
				  #x3ef56f10b39935f0
				  #x0b22d6829cd619c6
				  #x17fd460a74df2069
				  #x6cf8cc8e8510ed40
				  #xd6c824bf3a6ecaa7
				  #x61243d581a817049
				  #x048bacb6bbc163a2
				  #xd9a38ac27d44cc32
				  #x7fddff5baaf410ab
				  #xad6d495aa804824b
				  #xe1a6a74f2d8c9f94
				  #xd4f7851235dee8e3
				  #xfd4b7f886540d893
				  #x247c20042aa4bfda
				  #x096ea1c517d1327c
				  #xd56966b4361a6685
				  #x277da5c31221057d
				  #x94d59893a43acff7
				  #x64f0c51ccdc02281
				  #x3d33bcc4ff6189db
				  #xe005cb184ce66af1
				  #xff5ccd1d1db99bea
				  #xb0b854a7fe42980f
				  #x7bd46a6a718d4b9f
				  #xd10fa8cc22a5fd8c
				  #xd31484952be4bd31
				  #xc7fa975fcb243847
				  #x4886ed1e5846c407
				  #x28cddb791eb70b04
				  #xc2b00be2f573417f
				  #x5c9590452180f877
				  #x7a6bddfff370eb00
				  #xce509e38d6d9d6a4
				  #xebeb0f00647fa702
				  #x1dcc06cf76606f06
				  #xe4d9f28ba286ff0a
				  #xd85a305dc918c262
				  #x475b1d8732225f54
				  #x2d4fb51668ccb5fe
				  #xa679b9d9d72bba20
				  #x53841c0d912d43a5
				  #x3b7eaa48bf12a4e8
				  #x781e0e47f22f1ddf
				  #xeff20ce60ab50973
				  #x20d261d19dffb742
				  #x16a12b03062a2e39
				  #x1960eb2239650495
				  #x251c16fed50eb8b8
				  #x9ac0c330f826016e
				  #xed152665953e7671
				  #x02d63194a6369570
				  #x5074f08394b1c987
				  #x70ba598c90b25ce1
				  #x794a15810b9742f6
				  #x0d5925e9fcaf8c6c
				  #x3067716cd868744e
				  #x910ab077e8d7731b
				  #x6a61bbdb5ac42f61
				  #x93513efbf0851567
				  #xf494724b9e83e9d5
				  #xe887e1985c09648d
				  #x34b1d3c675370cfd
				  #xdc35e433bc0d255d
				  #xd0aab84234131be0
				  #x08042a50b48b7eaf
				  #x9997c4ee44a3ab35
				  #x829a7b49201799d0
				  #x263b8307b7c54441
				  #x752f95f4fd6a6ca6
				  #x927217402c08c6e5
				  #x2a8ab754a795d9ee
				  #xa442f7552f72943d
				  #x2c31334e19781208
				  #x4fa98d7ceaee6291
				  #x55c3862f665db309
				  #xbd0610175d53b1f3
				  #x46fe6cb840413f27
				  #x3fe03792df0cfa59
				  #xcfe700372eb85e8f
				  #xa7be29e7adbce118
				  #xe544ee5cde8431dd
				  #x8a781b1b41f1873e
				  #xa5c94c78a0d2f0e7
				  #x39412e2877b60728
				  #xa1265ef3afc9a62c
				  #xbcc2770c6a2506c5
				  #x3ab66dd5dce1ce12
				  #xe65499d04a675b37
				  #x7d8f523481bfd216
				  #x0f6f64fcec15f389
				  #x74efbe618b5b13c8
				  #xacdc82b714273e1d
				  #xdd40bfe003199d17
				  #x37e99257e7e061f8
				  #xfa52626904775aaa
				  #x8bbbf63a463d56f9
				  #xf0013f1543a26e64
				  #xa8307e9f879ec898
				  #xcc4c27a4150177cc
				  #x1b432f2cca1d3348
				  #xde1d1f8f9f6fa013
				  #x606602a047a7ddd6
				  #xd237ab64cc1cb2c7
				  #x9b938e7225fcd1d3
				  #xec4e03708e0ff476
				  #xfeb2fbda3d03c12d
				  #xae0bced2ee43889a
				  #x22cb8923ebfb4f43
				  #x69360d013cf7396d
				  #x855e3602d2d4e022
				  #x073805bad01f784c
				  #x33e17a133852f546
				  #xdf4874058ac7b638
				  #xba92b29c678aa14a
				  #x0ce89fc76cfaadcd
				  #x5f9d4e0908339e34
				  #xf1afe9291f5923b9
				  #x6e3480f60f4a265f
				  #xeebf3a2ab29b841c
				  #xe21938a88f91b4ad
				  #x57dfeff845c6d3c3
				  #x2f006b0bf62caaf2
				  #x62f479ef6f75ee78
				  #x11a55ad41c8916a9
				  #xf229d29084fed453
				  #x42f1c27b16b000e6
				  #x2b1f76749823c074
				  #x4b76eca3c2745360
				  #x8c98f463b91691bd
				  #x14bcc93cf1ade66a
				  #x8885213e6d458397
				  #x8e177df0274d4711
				  #xb49b73b5503f2951
				  #x10168168c3f96b6b
				  #x0e3d963b63cab0ae
				  #x8dfc4b5655a1db14
				  #xf789f1356e14de5c
				  #x683e68af4e51dac1
				  #xc9a84f9d8d4b0fd9
				  #x3691e03f52a0f9d1
				  #x5ed86e46e1878e80
				  #x3c711a0e99d07150
				  #x5a0865b20c4e9310
				  #x56fbfc1fe4f0682e
				  #xea8d5de3105edf9b
				  #x71abfdb12379187a
				  #x2eb99de1bee77b9c
				  #x21ecc0ea33cf4523
				  #x59a4d7521805c7a1
				  #x3896f5eb56ae7c72
				  #xaa638f3db18f75dc
				  #x9f39358dabe9808e
				  #xb7defa91c00b72ac
				  #x6b5541fd62492d92
				  #x6dc6dee8f92e4d5b
				  #x353f57abc4beea7e
				  #x735769d6da5690ce
				  #x0a234aa642391484
				  #xf6f9508028f80d9d
				  #xb8e319a27ab3f215
				  #x31ad9c1151341a4d
				  #x773c22a57bef5805
				  #x45c7561a07968633
				  #xf913da9e249dbe36
				  #xda652d9b78a64c68
				  #x4c27a97f3bc334ef
				  #x76621220e66b17f4
				  #x967743899acd7d0b
				  #xf3ee5bcae0ed6782
				  #x409f753600c879fc
				  #x06d09a39b5926db6
				  #x6f83aeb0317ac588
				  #x01e6ca4a86381f21
				  #x66ff3462d19f3025
				  #x72207c24ddfd3bfb
				  #x4af6b6d3e2ece2eb
				  #x9c994dbec7ea08de
				  #x49ace597b09a8bc4
				  #xb38c4766cf0797ba
				  #x131b9373c57c2a75
				  #xb1822cce61931e58
				  #x9d7555b909ba1c0c
				  #x127fafdd937d11d2
				  #x29da3badc66d92e4
				  #xa2c1d57154c2ecbc
				  #x58c5134d82f6fe24
				  #x1c3ae3515b62274f
				  #xe907c82e01cb8126
				  #xf8ed091913e37fcb
				  #x3249d8f9c80046c9
				  #x80cf9bede388fb63
				  #x1881539a116cf19e
				  #x5103f3f76bd52457
				  #x15b7e6f5ae47f7a8
				  #xdbd7c6ded47e9ccf
				  #x44e55c410228bb1a
				  #xb647d4255edb4e99
				  #x5d11882bb8aafc30
				  #xf5098bbb29d3212a
				  #x8fb5ea14e90296b3
				  #x677b942157dd025a
				  #xfb58e7c0a390acb5
				  #x89d3674c83bd4a01
				  #x9e2da4df4bf3b93b
				  #xfcc41e328cab4829
				  #x03f38c96ba582c52
				  #xcad1bdbd7fd85db2
				  #xbbb442c16082ae83
				  #xb95fe86ba5da9ab0
				  #xb22e04673771a93f
				  #x845358c9493152d8
				  #xbe2a488697b4541e
				  #x95a2dc2dd38e6966
				  #xc02c11ac923c852b
				  #x2388b1990df2a87b
				  #x7c8008fa1b4f37be
				  #x1f70d0c84d54e503
				  #x5490adec7ece57d4
				  #x002b3c27d9063a3a
				  #x7eaea3848030a2bf
				  #xc602326ded2003c0
				  #x83a7287d69a94086
				  #xc57a5fcb30f57a8a
				  #xb56844e479ebe779
				  #xa373b40f05dcbce9
				  #xd71a786e88570ee2
				  #x879cbacdbde8f6a0
				  #x976ad1bcc164a32f
				  #xab21e25e9666d78b
				  #x901063aae5e5c33c
				  #x9818b34448698d90
				  #xe36487ae3e1e8abb
				  #xafbdf931893bdcb4
				  #x6345a0dc5fbbd519
				  #x8628fe269b9465ca
				  #x1e5d01603f9c51ec
				  #x4de44006a15049b7
				  #xbf6c70e5f776cbb1
				  #x411218f2ef552bed
				  #xcb0c0708705a36a3
				  #xe74d14754f986044
				  #xcd56d9430ea8280e
				  #xc12591d7535f5065
				  #xc83223f1720aef96
				  #xc3a0396f7363a51f)))

(defun crysp_tiger (data)
  (if (not (arrayp data))
      (return-from crysp_tiger (make-array 3
					   :element-type '(unsigned-byte 64)
					   :initial-element 0)))

  (let ((h0 #x0123456789abcdef)
	(h1 #xfedcba9876543210)
	(h2 #xf096a5b4c3b2e187)))
)

(defun feedforward (a aa b bb c cc)
  (let ((bytes (make-array 3
			   :element-type '(unsigned-byte 64)
			   :initial-element 0)))
    (setf (aref bytes 0) (logior a aa))
    (setf (aref bytes 1) (- b bb))
    (setf (aref bytes 2) (+ c cc))
    bytes)
)

(defun key_schedule (x)
  (if (not (arrayp x))
      (return-from key_schedule (make-array 8
					    :element-type '(unsigned-byte 64)
					    :initial-element 0)))

  (setf (aref x 0) (- (aref x 0) (logior (aref x 7) #xa5a5a5a5a5a5a5a5)))
  (setf (aref x 1) (logior (aref x 0) (aref x 1)))
  (setf (aref x 2) (+ (aref x 1) (aref x 2)))
  (setf (aref x 3) (- (aref x 3) (logior (aref x 2) (ash (lognot x 1) 19))))
  (setf (aref x 4) (logior (aref x 3) (aref x 4)))
  (setf (aref x 5) (+ (aref x 4) (aref x 5)))
  (setf (aref x 6) (- (aref x 6) (logior (aref x 5) (ash (lognot x 4) -23))))
  (setf (aref x 7) (logior (aref x 6) (aref x 7)))
  (setf (aref x 0) (+ (aref x 0) (aref x 7)))
  (setf (aref x 1) (- (aref x 1) (logior (aref x 0) (ash (lognot x 7) 19))))
  (setf (aref x 2) (logior (aref x 1) (aref x 2)))
  (setf (aref x 3) (+ (aref x 2) (aref x 3)))
  (setf (aref x 4) (- (aref x 4) (logior (aref x 3) (ash (lognot x 2) -23))))
  (setf (aref x 5) (logior (aref x 4) (aref x 5)))
  (setf (aref x 6) (+ (aref x 5) (aref x 6)))
  (setf (aref x 7) (- (aref x 7) (logior (aref x 6) #x0123456789abcdef)))
  x
)

(defun pass (a b c mul x)
  (if (not (arrayp x))
      (return-from pass (make-array 3
				    :element-type '(unsigned-byte 64)
				    :initial-element 0)))

  (let ((bytes (make-array 3)
	       :element-type '(unsigned-byte 64)
	       :initial-element 0))
    (setf (aref bytes 0) a)
    (setf (aref bytes 1) b)
    (setf (aref bytes 2) c)
    (setf bytes (tiger_round (aref bytes 0)
			     (aref bytes 1)
			     (aref bytes 2)
			     (aref x 0)
			     mul))
    (setf bytes (tiger_round (aref bytes 1)
			     (aref bytes 2)
			     (aref bytes 0)
			     (aref x 1)
			     mul))
    (setf bytes (tiger_round (aref bytes 2)
			     (aref bytes 0)
			     (aref bytes 1)
			     (aref x 2)
			     mul))
    (setf bytes (tiger_round (aref bytes 0)
			     (aref bytes 1)
			     (aref bytes 2)
			     (aref x 3)
			     mul))
    (setf bytes (tiger_round (aref bytes 1)
			     (aref bytes 2)
			     (aref bytes 0)
			     (aref x 4)
			     mul))
    (setf bytes (tiger_round (aref bytes 2)
			     (aref bytes 0)
			     (aref bytes 1)
			     (aref x 5)
			     mul))
    (setf bytes (tiger_round (aref bytes 0)
			     (aref bytes 1)
			     (aref bytes 2)
			     (aref x 6)
			     mul))
    (setf bytes (tiger_round (aref bytes 1)
			     (aref bytes 2)
			     (aref bytes 0)
			     (aref x 7)
			     mul))
    bytes)
)

(defun process_block (h0 h1 h2 x)
  (let ((bytes (make-array 3
			   :element-type '(unsigned-byte 64)
			   :initial-element 0))
	(h (make-array 3
		       :element-type '(unsigned-byte 64)
		       :initial-element 0)))
    (setf (bytes aref 0) h0)
    (setf (bytes aref 1) h1)
    (setf (bytes aref 2) h2)
    (setf bytes (pass (aref bytes 0) (aref bytes 1) (aref bytes 2) 5 x))
    (setf x (key_schedule x))
    (setf bytes (pass (aref bytes 0) (aref bytes 1) (aref bytes 2) 7 x))
    (setf x (key_schedule x))
    (setf bytes (pass (aref bytes 0) (aref bytes 1) (aref bytes 2) 9 x))
    (setf (aref h 0) (logior (aref bytes 0) h0))
    (setf (aref h 1) (- (aref bytes 1) h1))
    (setf (aref h 2) (+ (aref bytes 2) h2))
    h)
)

(defun tiger_round (a b c x mul)
  (let ((aa a)
	(bb b)
	(bytes (make-array 3
			   :element-type '(unsigned-byte 64)
			   :initial-element 0))
	(cc c))
    (setf cc (logior cc x))
    (setf aa (- aa (logior (aref s_table_1 (logand cc #xff))
			   (aref s_table_2 (logand (ash cc -16) #xff))
			   (aref s_table_3 (logand (ash cc -32) #xff))
			   (aref s_table_4 (logand (ash cc -48) #xff)))))
    (setf bb (+ bb (logior (aref s_table_4 (logand (ash cc -8) #xff))
			   (aref s_table_3 (logand (ash cc -24) #xff))
			   (aref s_table_2 (logand (ash cc -40) #xff))
			   (aref s_table_1 (logand (ash cc -56) #xff)))))
    (setf bb (* bb mul))
    (setf (aref bytes 0) aa)
    (setf (aref bytes 1) bb)
    (setf (aref bytes 2) cc)
    bytes)
)
