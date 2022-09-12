package com.unper.samper.domain.dao;

import com.unper.samper.constant.EPermissionStatus;
import com.unper.samper.domain.common.Auditor;
import lombok.*;

import javax.persistence.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "permission", schema = "public")
public class Permission extends Auditor {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "user_id")
    private User student;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = Schedule.class)
    @JoinColumn(name = "schedule_id")
    private Schedule schedule;

    @Column(name = "permission_detail", nullable = false)
    private String permissionDetail;

    @Column(name = "evidence")
    private String evidence;

    @Enumerated(EnumType.STRING)
    private EPermissionStatus status;

}
